-- | The SQL statements inside PL/pgSQL function bodies.
--
-- To PostgreSQL's SQL parser a function body is one string constant, so the
-- file-level pass leaves it byte for byte. This pass parses the body as
-- PL/pgSQL (libpg_query's @pg_query_parse_plpgsql@) and lays out the plain SQL
-- statements in it: statements run for their effect
-- (@PLpgSQL_stmt_execsql@) and @RETURN QUERY@. The PL/pgSQL around them
-- (@DECLARE@, @IF@, loops, assignments) is left as it is, as are statements
-- with an @INTO@ clause (PL/pgSQL takes it out of the query text, so the
-- query is no longer what the source says).
--
-- libpg_query gives each statement's line within the body and its source
-- text, so a statement is found by searching for its text from the start of
-- its line.
--
-- The result is checked on its own: the function's PL/pgSQL parse before and
-- after must match once line numbers are dropped and every embedded query is
-- replaced by its normal form.
module PgFmt.Plpgsql (
    formatBodies,
) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((^.))
import PgFmt.LibPgQuery (Token (..))
import PgFmt.LibPgQuery qualified as LibPgQuery
import PgQuery qualified as Pg


-- | An embedded statement to lay out: its line within the body (1 is the line
-- the body starts on) and its text.
data Target = Target
    { lineNo :: Int
    , query :: ByteString
    }


-- | Lay out the SQL statements in a statement's PL/pgSQL function body.
--
-- The callback lays out one embedded statement, given the column its line is
-- indented to: 'Right' 'Nothing' leaves it as it is, 'Left' is a formatter
-- bug. The result is the statement with its new body, 'Nothing' when nothing
-- changed (or the statement is not a PL/pgSQL function), or 'Left' why the
-- new body was refused.
formatBodies ::
    (Int -> ByteString -> IO (Either Text (Maybe Text))) ->
    ByteString ->
    IO (Either Text (Maybe ByteString))
formatBodies layOut stmt = do
    parsed <- LibPgQuery.parsePlpgsql stmt
    range <- bodyRange stmt
    case (parsed >>= decodeJson, range) of
        (Right [fn], Just (bodyStart, bodyEnd)) -> do
            let body = slice bodyStart bodyEnd stmt
                located = mapMaybe (locate body) (targets fn)
            laidOut <- traverse (layOutOne body) located
            case sequence laidOut of
                Left reason -> pure (Left reason)
                Right edits -> case [e | Just e <- edits] of
                    [] -> pure (Right Nothing)
                    changes -> do
                        let newBody = splice body changes
                            newStmt = BS.take bodyStart stmt <> newBody <> BS.drop bodyEnd stmt
                        same <- samePlpgsql stmt newStmt
                        pure case same of
                            Nothing -> Right (Just newStmt)
                            Just reason -> Left ("PL/pgSQL body: " <> reason)
        -- Not a PL/pgSQL function, one libpg_query cannot compile, or a body
        -- this pass cannot find: leave it.
        _ -> pure (Right Nothing)
  where
    layOutOne body (start, end, column) = do
        result <- layOut column (slice start end body)
        pure case result of
            Left reason -> Left reason
            Right Nothing -> Right Nothing
            Right (Just new)
                | TE.encodeUtf8 new == slice start end body -> Right Nothing
                | otherwise -> Right (Just (start, end, TE.encodeUtf8 new))


decodeJson :: ByteString -> Either LibPgQuery.LibPgQueryError [Value]
decodeJson bytes = case Aeson.eitherDecodeStrict bytes of
    Right (Array fns) -> Right (toList fns)
    Right _ -> Left LibPgQuery.LibPgQueryError{message = "PL/pgSQL JSON is not an array", cursor = 0}
    Left err -> Left LibPgQuery.LibPgQueryError{message = T.pack err, cursor = 0}


-- | The byte range of the function body: the inside of the statement's one
-- dollar-quoted string. Bodies in single quotes, and statements with more
-- than one dollar-quoted string, are not handled.
bodyRange :: ByteString -> IO (Maybe (Int, Int))
bodyRange stmt = do
    scanned <- LibPgQuery.scan stmt
    pure case scanned of
        Right result
            | [t] <- filter dollarQuoted (LibPgQuery.tokens result)
            , Just tagEnd <- BS.elemIndex 36 (BS.drop (t.start + 1) stmt) ->
                -- The tag is @$tag$@: from t.start to the second @$@.
                let tagLen = tagEnd + 2
                 in Just (t.start + tagLen, t.end - tagLen)
        _ -> Nothing
  where
    dollarQuoted t = not t.isComment && BS.take 1 (BS.drop t.start stmt) == "$" && BS.take 1 (BS.drop (t.end - 1) stmt) == "$"
            && t.end - t.start >= 4


-- | The statements of a function to lay out.
targets :: Value -> [Target]
targets = go
  where
    go = \case
        Object o ->
            let here = case (KeyMap.lookup "PLpgSQL_stmt_execsql" o, KeyMap.lookup "PLpgSQL_stmt_return_query" o) of
                    (Just (Object s), _)
                        | KeyMap.lookup "into" s /= Just (Bool True) ->
                            target s (field "sqlstmt" s)
                    (_, Just (Object s)) -> target s (field "query" s)
                    _ -> []
             in here <> concatMap go (KeyMap.elems o)
        Array xs -> concatMap go (toList xs)
        _ -> []
    field k s = KeyMap.lookup (Key.fromText k) s
    target s expr = case (KeyMap.lookup "lineno" s, expr >>= exprQuery) of
        (Just (Number n), Just q) -> [Target{lineNo = round n, query = TE.encodeUtf8 q}]
        _ -> []
    exprQuery = \case
        Object e | Just (Object x) <- KeyMap.lookup "PLpgSQL_expr" e, Just (String q) <- KeyMap.lookup "query" x -> Just q
        _ -> Nothing


-- | Where a target's text is in the body, and the indentation of the line it
-- starts on.
locate :: ByteString -> Target -> Maybe (Int, Int, Int)
locate body t = do
    lineStart <- nthLineStart t.lineNo
    let (before, rest) = BS.breakSubstring t.query (BS.drop lineStart body)
    if BS.null rest
        then Nothing
        else do
            let start = lineStart + BS.length before
                startOfLine = maybe 0 (+ 1) (BS.elemIndexEnd 10 (BS.take start body))
                column = BS.length (BC.takeWhile (== ' ') (BS.drop startOfLine body))
            Just (start, start + BS.length t.query, column)
  where
    lineStarts = 0 : map (+ 1) (BS.elemIndices 10 body)
    nthLineStart n
        | n >= 1, (s : _) <- drop (n - 1) lineStarts = Just s
        | otherwise = Nothing


-- | Replace byte ranges of the body, in order.
splice :: ByteString -> [(Int, Int, ByteString)] -> ByteString
splice body changes = BS.concat (go 0 (sortOn (\(s, _, _) -> s) changes))
  where
    go pos [] = [BS.drop pos body]
    go pos ((s, e, new) : rest) = slice pos s body : new : go e rest


-- | 'Nothing' when both statements' PL/pgSQL parses match once line numbers
-- are dropped and every query is replaced by its normal form.
samePlpgsql :: ByteString -> ByteString -> IO (Maybe Text)
samePlpgsql before after = do
    a <- LibPgQuery.parsePlpgsql before
    b <- LibPgQuery.parsePlpgsql after
    case (a >>= decodeJson, b >>= decodeJson) of
        (Right fa, Right fb) -> do
            na <- traverse normalise fa
            nb <- traverse normalise fb
            pure if na == nb then Nothing else Just "the formatted body is a different function"
        (_, Left err) -> pure (Just ("the formatted body does not parse: " <> err.message))
        (Left err, _) -> pure (Just ("the original body does not parse: " <> err.message))


normalise :: Value -> IO Value
normalise = \case
    Object o -> Object <$> KeyMap.traverseWithKey step (KeyMap.delete "lineno" o)
    Array xs -> Array <$> traverse normalise xs
    v -> pure v
  where
    step k v
        | k == "query", String q <- v = String <$> normalForm q
        | otherwise = normalise v
    -- A query that parses as SQL compares by its normal form; anything else
    -- (an expression such as @v > 1@) compares as written.
    normalForm q = do
        parsed <- LibPgQuery.parse (TE.encodeUtf8 q)
        case parsed of
            Right tree | [raw] <- tree ^. Pg.stmts -> do
                c <- LibPgQuery.canonical raw
                pure (either (const q) id c)
            _ -> pure q


slice :: Int -> Int -> ByteString -> ByteString
slice a b = BS.take (b - a) . BS.drop a

