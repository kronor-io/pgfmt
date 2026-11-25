-- | Formatting a whole SQL file: libpg_query splits it into statements, each
-- statement is formatted on its own by the chosen backend, and everything
-- between statements (whitespace, comments, semicolons) is kept byte for byte.
--
-- No statement is rewritten unless libpg_query, PostgreSQL's own parser, reads
-- the result as the same statement: its normal form ('canonical') before and
-- after must match. That check is independent of either backend's own.
module PgFmt.File (
    Backend (..),
    KeywordCase (..),
    Options (..),
    defaultOptions,
    Outcome (..),
    StatementReport (..),
    FileResult (..),
    formatFile,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((^.))
import PgFmt.LibPgQuery (LibPgQueryError (..), PrettyOptions, Statement (..), Token (..))
import PgFmt.LibPgQuery qualified as LibPgQuery
import PgFmt.Render (Dialect (..), FormatError (..))
import PgFmt.Render qualified as Render
import PgQuery qualified as Pg


-- | What lays out a statement.
data Backend
    = -- | pgfmt's own renderer over postgresql-syntax. DML only; statements it
      -- does not parse, or that have comments inside them, are left as they are.
      Syntax
    | -- | libpg_query's pretty-printer. Any statement PostgreSQL parses,
      -- comments included.
      LibPgQuery
    | -- | 'Syntax' where it applies, 'LibPgQuery' for the rest.
      Auto
    deriving stock (Eq, Show)


-- | How every formatted statement spells its keywords, whichever backend
-- laid it out. Identifiers are never touched.
data KeywordCase = Lower | Upper
    deriving stock (Eq, Show)


data Options = Options
    { backend :: Backend
    , dialect :: Dialect
    , keywordCase :: KeywordCase
    , pretty :: PrettyOptions
    }
    deriving stock (Eq, Show)


defaultOptions :: Options
defaultOptions =
    Options
        { backend = Auto
        , dialect = PlainSql
        , keywordCase = Lower
        , pretty = LibPgQuery.defaultPrettyOptions
        }


-- | What happened to one statement.
data Outcome
    = -- | Rewritten by this backend (unchanged text counts too).
      FormattedBy Backend
    | -- | Left as it was, for this reason. Not an error.
      Kept Text
    | -- | A backend produced SQL that PostgreSQL reads as a different
      -- statement, or not at all. The statement is left as it was, but this is
      -- a formatter bug and the caller should fail.
      Failed Text
    deriving stock (Eq, Show)


data StatementReport = StatementReport
    { line :: Int
    -- ^ 1-based line the statement starts on.
    , outcome :: Outcome
    }
    deriving stock (Eq, Show)


data FileResult = FileResult
    { output :: Text
    , reports :: [StatementReport]
    }
    deriving stock (Eq, Show)


-- | Format every statement of a file. 'Left' when PostgreSQL cannot parse the
-- file at all (it is invalid SQL, or holds psql meta-commands).
formatFile :: Options -> Text -> IO (Either LibPgQueryError FileResult)
formatFile opts text = do
    let source = TE.encodeUtf8 text
    parsed <- LibPgQuery.parse source
    scanned <- LibPgQuery.scan source
    case (,) <$> parsed <*> scanned of
        Left err -> pure (Left err)
        Right (tree, scanResult) -> do
            let toks = LibPgQuery.tokens scanResult
            let stmts = LibPgQuery.statements source tree
            results <- traverse (formatOne opts source toks) stmts
            let r = assemble source results
            -- Each statement was checked on its own; check the file as a
            -- whole too, which also catches text between statements that a
            -- rewrite changed the meaning of (a semicolon commented out).
            same <- sameStatements (map (.raw) stmts) r.output
            pure . Right $ case same of
                Nothing -> r
                Just reason ->
                    FileResult
                        { output = text
                        , reports = r.reports <> [StatementReport{line = 1, outcome = Failed ("the formatted file " <> reason)}]
                        }


-- | 'Nothing' when the file parses to statements with these normal forms.
sameStatements :: [Pg.RawStmt] -> Text -> IO (Maybe Text)
sameStatements originals output = do
    reparsed <- LibPgQuery.parse (TE.encodeUtf8 output)
    case reparsed of
        Left err -> pure (Just ("does not parse: " <> err.message))
        Right tree -> do
            before <- traverse LibPgQuery.canonical originals
            after <- traverse LibPgQuery.canonical (tree ^. Pg.stmts)
            pure
                if sequence before == sequence after
                    then Nothing
                    else Just "holds different statements than the original"


-- | A statement's new text for the byte range it replaces.
data Replacement = Replacement
    { from :: Int
    , to :: Int
    , text :: Maybe Text
    , report :: StatementReport
    }


assemble :: ByteString -> [Replacement] -> FileResult
assemble source replacements =
    FileResult
        { output = T.concat (go 0 replacements)
        , reports = map (.report) replacements
        }
  where
    slice a b = decode (BS.take (b - a) (BS.drop a source))
    go pos [] = [slice pos (BS.length source)]
    go pos (r : rs) = case r.text of
        Nothing -> go pos rs
        Just new -> slice pos r.from : new : go r.to rs


formatOne :: Options -> ByteString -> [Token] -> Statement -> IO Replacement
formatOne opts source toks stmt =
    case [t | t <- toks, t.start >= stmt.start, t.end <= stmt.end] of
        [] -> pure (keep stmt.start stmt.end "empty statement")
        inside -> do
            -- The statement proper: from its first token that is not a comment
            -- to its last. Comments and whitespace on either side stay where
            -- they are.
            let code = dropWhileEnd (.isComment) (dropWhile (.isComment) inside)
                bodyStart = case code of
                    t : _ -> t.start
                    [] -> stmt.end
                bodyEnd = maximum (bodyStart : map (.end) code)
                body = BS.take (bodyEnd - bodyStart) (BS.drop bodyStart source)
                interiorComments = any (.isComment) code
                lineNo = 1 + BS.count 10 (BS.take bodyStart source)
                -- The column the statement starts at, when only whitespace
                -- precedes it on its line: its other lines line up with it.
                lineBefore = decode (snd (BS.breakEnd (== 10) (BS.take bodyStart source)))
                column = if T.all (== ' ') lineBefore then T.length lineBefore else 0
                done outcome new =
                    Replacement
                        { from = bodyStart
                        , to = bodyEnd
                        , text = new
                        , report = StatementReport{line = lineNo, outcome}
                        }
            if null code
                then pure (done (Kept "only comments") Nothing)
                else do
                    attempt <- runBackend opts body interiorComments
                    case attempt of
                        Left outcome -> pure (done outcome Nothing)
                        Right (used, formatted0) -> do
                            cased <- applyKeywordCase opts used body formatted0
                            case cased of
                                Left reason -> pure (done (Failed reason) Nothing)
                                Right formatted1 -> do
                                    formatted <- endLineComment =<< indentContinuation column formatted1
                                    verdict <- verify stmt.raw (TE.encodeUtf8 formatted) (countComments code)
                                    pure case verdict of
                                        Nothing -> done (FormattedBy used) (Just formatted)
                                        Just reason -> done (Failed reason) Nothing
  where
    keep a b reason =
        Replacement
            { from = a
            , to = b
            , text = Nothing
            , report = StatementReport{line = 1 + BS.count 10 (BS.take a source), outcome = Kept reason}
            }
    countComments = length . filter (.isComment)


-- | Lay out one statement, or say why not.
runBackend :: Options -> ByteString -> Bool -> IO (Either Outcome (Backend, Text))
runBackend opts body interiorComments = case opts.backend of
    Syntax -> pure (syntax body)
    LibPgQuery -> libpgQuery body
    Auto -> case syntax body of
        Right ok -> pure (Right ok)
        Left (Failed reason) -> pure (Left (Failed reason))
        Left (Kept _) -> libpgQuery body
        Left other -> pure (Left other)
  where
    syntax sql
        -- postgresql-syntax drops comments, and its AST check cannot see that.
        | interiorComments = Left (Kept "comments inside the statement")
        | otherwise = case Render.formatStatement opts.dialect (decode sql) of
            Right formatted -> Right (Syntax, formatted)
            Left (Unsupported _) -> Left (Kept "postgresql-syntax does not parse it")
            Left (RendererBug reason) -> Left (Failed reason)
    libpgQuery sql = do
        parsed <- LibPgQuery.parse sql
        case parsed of
            Left err -> pure (Left (Failed ("libpg_query could not re-parse the statement: " <> err.message)))
            Right tree -> do
                printed <- LibPgQuery.deparsePretty opts.pretty (Just sql) tree
                pure case printed of
                    Left err -> Left (Kept ("libpg_query could not deparse it: " <> err.message))
                    Right formatted -> Right (LibPgQuery, T.strip formatted)

-- | Spell the keywords of a backend's output in the chosen case.
--
-- Telling keywords from identifiers spelled like keywords (@name@, @type@):
-- libpg_query writes keywords in upper case and unquoted identifiers in lower
-- case, so its upper-case keyword tokens are keywords. For the syntax backend,
-- the statement is rendered again
-- with its identifiers masked ('Render.renderIdentifiersMasked'); the tokens
-- that differ between the two renderings are identifiers.
applyKeywordCase :: Options -> Backend -> ByteString -> Text -> IO (Either Text Text)
applyKeywordCase opts used body formatted = do
    let bytes = TE.encodeUtf8 formatted
    scanned <- LibPgQuery.scan bytes
    case scanned of
        Left err -> pure (Left ("formatted SQL does not scan: " <> err.message))
        Right result -> do
            let toks = LibPgQuery.tokens result
                textOf src t = decode (BS.take (t.end - t.start) (BS.drop t.start src))
            keywords <- case used of
                Syntax -> case Render.renderIdentifiersMasked opts.dialect (decode body) of
                    Left _ -> pure (Left "could not render the statement's identifiers")
                    Right marked -> do
                        let markedBytes = TE.encodeUtf8 marked
                        markedScan <- LibPgQuery.scan markedBytes
                        pure case markedScan of
                            Right m
                                | let markedToks = LibPgQuery.tokens m
                                , length markedToks == length toks ->
                                    Right
                                        [ t
                                        | (t, t') <- zip toks markedToks
                                        , t.isKeyword
                                        , textOf bytes t == textOf markedBytes t'
                                        ]
                            _ -> Left "could not tell keywords from identifiers"
                _ ->
                    pure . Right $
                        [ t
                        | t <- toks
                        , t.isKeyword
                        , let word = textOf bytes t
                        , T.toUpper word == word
                        ]
            pure (rewrite bytes textOf <$> keywords)
  where
    recase = case opts.keywordCase of
        Lower -> T.toLower
        Upper -> T.toUpper
    rewrite bytes textOf keywords =
        let go pos [] = [decode (BS.drop pos bytes)]
            go pos (t : ts) =
                decode (BS.take (t.start - pos) (BS.drop pos bytes)) : recase (textOf bytes t) : go t.end ts
         in T.concat (go 0 keywords)


-- | Indent every line but the first by this many spaces, except lines that
-- start inside a token (a multi-line string or function body), whose text
-- that would change.
indentContinuation :: Int -> Text -> IO Text
indentContinuation 0 formatted = pure formatted
indentContinuation column formatted = do
    let bytes = TE.encodeUtf8 formatted
    scanned <- LibPgQuery.scan bytes
    let toks = either (const []) LibPgQuery.tokens scanned
        insideToken p = any (\t -> t.start < p && p < t.end) toks
        -- Byte offset at which each line starts.
        starts = scanl (\acc l -> acc + BS.length l + 1) 0 (BS.split 10 bytes)
        pad (start, l)
            | start == 0 || BS.null l || insideToken start = l
            | otherwise = BS.replicate column 32 <> l
    pure case scanned of
        -- Not scannable: the verification after this rejects it anyway.
        Left _ -> formatted
        Right _ -> decode (BS.intercalate "\n" (map pad (zip starts (BS.split 10 bytes))))


-- | A line comment at the end of a statement would swallow whatever follows
-- it on the line, the semicolon first. libpg_query moves comments it cannot
-- place to the end, so end the line after one.
endLineComment :: Text -> IO Text
endLineComment formatted = do
    scanned <- LibPgQuery.scan (TE.encodeUtf8 formatted)
    pure case scanned of
        Right result
            | t : _ <- reverse (LibPgQuery.tokens result)
            , t.isComment
            , "--" `T.isPrefixOf` decode (BS.drop t.start (TE.encodeUtf8 formatted)) ->
                formatted <> "\n"
        _ -> formatted


-- | 'Nothing' when the formatted SQL is one statement with the original's
-- normal form and as many comments; otherwise why not.
verify :: Pg.RawStmt -> ByteString -> Int -> IO (Maybe Text)
verify original formatted commentCount = do
    before <- LibPgQuery.canonical original
    reparsed <- LibPgQuery.parse formatted
    rescanned <- LibPgQuery.scan formatted
    case (before, reparsed, rescanned) of
        (Left err, _, _) -> pure (Just ("libpg_query could not deparse the original: " <> err.message))
        (_, Left err, _) -> pure (Just ("formatted SQL does not parse: " <> err.message))
        (_, _, Left err) -> pure (Just ("formatted SQL does not scan: " <> err.message))
        (Right expected, Right tree, Right scanResult) -> case tree ^. Pg.stmts of
            [raw] -> do
                after <- LibPgQuery.canonical raw
                let comments = length (filter (.isComment) (LibPgQuery.tokens scanResult))
                pure case after of
                    Left err -> Just ("libpg_query could not deparse the formatted SQL: " <> err.message)
                    Right actual
                        | actual /= expected ->
                            Just ("formatted SQL is a different statement:\n  before: " <> expected <> "\n  after:  " <> actual)
                        | comments /= commentCount ->
                            Just ("formatting lost comments: " <> T.pack (show commentCount) <> " before, " <> T.pack (show comments) <> " after")
                        | otherwise -> Nothing
            raws -> pure (Just ("formatted SQL is " <> T.pack (show (length raws)) <> " statements"))


decode :: ByteString -> Text
decode = TE.decodeUtf8Lenient
