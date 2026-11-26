-- | Formatting the SQL in a Haskell file's hasql-th quasiquotes.
--
-- A small scanner finds the quasiquotes, skipping comments, string and
-- character literals the way GHC's lexer does, so a @[singletonStatement|@
-- inside a string or a comment is not mistaken for one. Each quasiquote whose
-- quoter is in 'Quoters' has its SQL laid out by "PgFmt.Render", which only
-- returns SQL that parses back to the same postgresql-syntax AST. Everything
-- else in the file is kept as it is, and the result is scanned again to check
-- that it holds the same quasiquotes and the same text around them.
module PgFmt.Haskell (
    Quoters,
    defaultQuoters,
    QuasiQuote (..),
    quasiQuotes,
    formatHaskell,
) where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import PgFmt.File (FileResult (..), Outcome (..), StatementReport (..))
import PgFmt.File qualified as File
import PgFmt.Render (FormatError (..))
import PgFmt.Render qualified as Render


-- | The quoters whose quasiquotes hold SQL to format, by unqualified name: a
-- qualified @[TH.singletonStatement|@ counts as @singletonStatement@.
type Quoters = [Text]


-- | hasql-th's statement quoters, and 'uncheckedSql'.
defaultQuoters :: Quoters
defaultQuoters =
    [ "singletonStatement"
    , "maybeStatement"
    , "vectorStatement"
    , "foldStatement"
    , "resultlessStatement"
    , "rowsAffectedStatement"
    , "uncheckedSql"
    ]


-- | One quasiquote, @[quoter|body|]@. Offsets count characters from the start
-- of the file.
data QuasiQuote = QuasiQuote
    { quoter :: Text
    -- ^ As written, qualified or not.
    , bodyStart :: Int
    -- ^ Offset of the body's first character, just after the opening @|@.
    , bodyEnd :: Int
    -- ^ Offset of the closing @|]@.
    , line :: Int
    -- ^ 1-based line of the opening @[@.
    , column :: Int
    -- ^ 0-based column of the opening @[@.
    }
    deriving stock (Eq, Show)


-- | Every quasiquote in a Haskell source file, in order.
--
-- As in GHC with QuasiQuotes on, @[name|@ with no space before the bar opens
-- one, and the first @|]@ closes it; @[e|@, @[d|@, @[t|@ and @[p|@ are
-- Template Haskell brackets, whose contents are Haskell. A @[name|@ with no
-- @|]@ after it is taken for a list comprehension.
quasiQuotes :: Text -> [QuasiQuote]
quasiQuotes source = code 0 1 0 Nothing (T.unpack source)
  where
    -- offset, line, column, the character before, the rest.
    code :: Int -> Int -> Int -> Maybe Char -> String -> [QuasiQuote]
    code _ _ _ _ [] = []
    code i l c prev s@(x : rest)
        | x == '{', '-' : _ <- rest = blockComment (i + 2) l (c + 2) (1 :: Int) (drop 1 rest)
        | x == '-', lineComment prev s = skipLine i l c s
        | x == '"' = string (i + 1) l (c + 1) rest
        | x == '\'' = charLiteral
        | x == '[', Just (name, afterBar) <- opening rest =
            let open = i + 1 + T.length name + 1
             in case closing open afterBar of
                    Just (end, afterClose)
                        | name `notElem` ["e", "d", "t", "p"] ->
                            let body = take (end - open) afterBar
                                (l', c') = advance (l, c) ('[' : T.unpack name <> "|" <> body <> "|]")
                             in QuasiQuote{quoter = name, bodyStart = open, bodyEnd = end, line = l, column = c}
                                    : code (end + 2) l' c' (Just ']') afterClose
                    _ -> step
        | otherwise = step
      where
        step = let (l', c') = advance (l, c) [x] in code (i + 1) l' c' (Just x) rest
        -- A prime after an identifier character is part of the name (x', f'');
        -- ''T quotes a type name; otherwise a character literal is 'x' or an
        -- escape, '\n', and anything else is a quoted name, 'f.
        charLiteral
            | Just p <- prev, isIdentChar p = step
            | '\'' : more <- rest = code (i + 2) l (c + 2) (Just '\'') more
            -- The character after the backslash is part of the escape even
            -- when it is a quote, '\''.
            | '\\' : _ : more <- rest =
                let (escape, after) = break (== '\'') more
                    n = 3 + length escape + 1
                 in code (i + n) l (c + n) (Just '\'') (drop 1 after)
            | _ : '\'' : more <- rest = code (i + 3) l (c + 3) (Just '\'') more
            | otherwise = step

    skipLine i l _ s =
        let (comment, after) = break (== '\n') s
         in case after of
                [] -> []
                _ : more -> code (i + length comment + 1) (l + 1) 0 (Just '\n') more

    blockComment i l c depth s = case s of
        [] -> []
        '-' : '}' : more
            | depth == 1 -> code (i + 2) l (c + 2) (Just '}') more
            | otherwise -> blockComment (i + 2) l (c + 2) (depth - 1) more
        '{' : '-' : more -> blockComment (i + 2) l (c + 2) (depth + 1) more
        x : more -> let (l', c') = advance (l, c) [x] in blockComment (i + 1) l' c' depth more

    string i l c s = case s of
        [] -> []
        '"' : more -> code (i + 1) l (c + 1) (Just '"') more
        -- A string gap, \  \, spans whitespace (newlines too) between two
        -- backslashes.
        '\\' : w : more
            | isSpace w ->
                let (gap, after) = break (== '\\') (w : more)
                    (l', c') = advance (l, c + 1) gap
                 in string (i + 1 + length gap + 1) l' (c' + 1) (drop 1 after)
        '\\' : e : more -> let (l', c') = advance (l, c + 1) [e] in string (i + 2) l' c' more
        -- An unterminated string ends at the line; GHC would reject the file.
        '\n' : more -> code (i + 1) (l + 1) 0 (Just '\n') more
        x : more -> let (l', c') = advance (l, c) [x] in string (i + 1) l' c' more


-- | Whether a @-@ at this point starts a line comment: two or more dashes
-- that are not part of an operator such as @-->@ or @|--@.
lineComment :: Maybe Char -> String -> Bool
lineComment prev s =
    let (dashes, after) = span (== '-') s
     in length dashes >= 2
            && maybe True (not . isSymbol) prev
            && case after of
                [] -> True
                y : _ -> not (isSymbol y)


isSymbol :: Char -> Bool
isSymbol x = x `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)


isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || x == '_' || x == '\''


-- | @name|@ after a @[@, the name possibly qualified, with no space before
-- the bar: the name and what follows the bar.
opening :: String -> Maybe (Text, String)
opening s =
    let (name, after) = span (\x -> isIdentChar x || x == '.') s
     in case after of
            '|' : rest | validQuoter name -> Just (T.pack name, rest)
            _ -> Nothing
  where
    validQuoter name = case splitDots name of
        [] -> False
        parts ->
            all qualifier (init parts) && variable (last parts)
    qualifier p = case p of
        x : _ -> isUpper x
        [] -> False
    variable p = case p of
        x : _ -> isLower x || x == '_'
        [] -> False
    splitDots = map T.unpack . T.splitOn "." . T.pack


-- | The offset of the first @|]@ at or after this one, and what follows it.
closing :: Int -> String -> Maybe (Int, String)
closing i = \case
    [] -> Nothing
    '|' : ']' : more -> Just (i, more)
    _ : more -> closing (i + 1) more


-- | Line and column after these characters.
advance :: (Int, Int) -> String -> (Int, Int)
advance = foldl \(l, c) x -> if x == '\n' then (l + 1, 0) else (l, c + 1)


-- | The unqualified name of a quoter.
baseName :: Text -> Text
baseName = snd . T.breakOnEnd "."


-- | Format the SQL of every quasiquote of these quoters. Each gets a report,
-- at the line of its opening bracket. A quasiquote whose SQL would come out
-- meaning something else is left as it was and reported as 'Failed'.
formatHaskell :: Quoters -> Text -> FileResult
formatHaskell quoters source =
    let found = zip [0 :: Int ..] (quasiQuotes source)
        results = [(n, q, one q) | (n, q) <- found, baseName q.quoter `elem` quoters]
        edits = [(q, new) | (_, q, (Just new, _)) <- results]
        rewritten = [n | (n, _, (Just _, _)) <- results]
        output = splice source edits
        reports = [StatementReport{line = q.line, outcome = o} | (_, q, (_, o)) <- results]
     in case sameOutside source output rewritten of
            Nothing -> FileResult{output, reports}
            Just reason ->
                FileResult
                    { output = source
                    , reports = reports <> [StatementReport{line = 1, outcome = Failed ("the formatted file " <> reason)}]
                    }
  where
    one q =
        let body = T.take (q.bodyEnd - q.bodyStart) (T.drop q.bodyStart source)
         in if
                | "\r" `T.isInfixOf` body -> (Nothing, Kept "CRLF line endings")
                | otherwise -> case Render.formatQuasiQuote q.column body of
                    Right Nothing -> (Nothing, Kept "single-line quasiquotes are left as they are")
                    Right (Just new) -> (Just new, FormattedBy File.Syntax)
                    Left (Unsupported _) -> (Nothing, Kept "postgresql-syntax does not parse it")
                    Left (RendererBug reason) -> (Nothing, Failed reason)


-- | Replace the bodies of these quasiquotes.
splice :: Text -> [(QuasiQuote, Text)] -> Text
splice source edits = go 0 (sortOn (\(q, _) -> q.bodyStart) edits)
  where
    go from = \case
        [] -> T.drop from source
        (q, new) : more ->
            T.take (q.bodyStart - from) (T.drop from source) <> new <> go q.bodyEnd more


-- | 'Nothing' when the output has the same quasiquotes as the source, with
-- the same text outside the bodies of the rewritten ones (these indices into
-- 'quasiQuotes').
sameOutside :: Text -> Text -> [Int] -> Maybe Text
sameOutside source output rewritten =
    let before = quasiQuotes source
        after = quasiQuotes output
     in if
            | map (.quoter) before /= map (.quoter) after -> Just "has different quasiquotes"
            | outside source before /= outside output after -> Just "changed text outside the quasiquotes"
            | otherwise -> Nothing
  where
    -- The text with the rewritten quasiquotes' bodies cut out.
    outside text qqs = go 0 [q | (n, q) <- zip [0 ..] qqs, n `elem` rewritten]
      where
        go from = \case
            [] -> [T.drop from text]
            q : more -> T.take (q.bodyStart - from) (T.drop from text) : go q.bodyEnd more
