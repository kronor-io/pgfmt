-- | Comments: each is written at the line break the layout makes where it
-- is in the source. One on a line with code ends the line before the break;
-- one on a line of its own gets a line of its own after it. A construct
-- with a comment inside it takes its multi-line form. A comment with no
-- break at its place is not written, and the statement is refused.
--
-- Also the token spans and lists comments are looked for in.
module PgFmt.Layout.Comments where

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import PgFmt.Layout.Locations
import PgFmt.Layout.Monad
import PgQuery qualified as Pg
import Prettyprinter (hardline)


-- | The comments in the gap before a token: after the token before it, and
-- before that the comma before it if any (a list's separator, which the
-- source may have before or after a comment).
gapBefore :: Int -> R [Cmt]
gapBefore q = do
    m <- asks (.cmts)
    sep <- tokIs (q - 1) ","
    let at p = IM.findWithDefault [] p m
    pure ((if sep then at (q - 2) else []) ++ at (q - 1))


-- | A line break before the token at @q@, with the comments before it;
-- @lead@ starts each comment line, aligning it with what follows. A blank
-- line before a comment on a line of its own is kept, as it separates
-- comments about different things.
brk :: D -> Int -> R D
brk lead q = do
    cs <- gapBefore q
    let (trailing, rest) = span (not . (.ownLine)) cs
    pure (mconcat [" " <> raw c.ctext | c <- trailing] <> hardline <> mconcat [(if c.blankBefore then hardline else mempty) <> lead <> raw c.ctext <> hardline | c <- rest])


-- | 'brk' where the token is known, a plain line break where not.
brkAt :: D -> Maybe Int -> R D
brkAt lead = maybe (pure hardline) (brk lead)


-- | Lines, each with the token it starts at: the comments between them go
-- at the breaks.
linesAt :: D -> [(Maybe Int, D)] -> R D
linesAt _ [] = pure mempty
linesAt lead ((_, d) : rest) = do
    ds <- forM rest \(q, x) -> (<> x) <$> brkAt lead q
    pure (d <> mconcat ds)


-- | Whether any comment lies between two tokens.
commentsBetween :: Int -> Int -> R Bool
commentsBetween a b = do
    m <- asks (.cmts)
    pure case IM.lookupGE a m of
        Just (p, _) -> p < b
        Nothing -> False


-- | Whether the statement has comments at all.
anyComments :: R Bool
anyComments = asks (not . IM.null . (.cmts))


-- | The tokens an expression spans: from its first located token to its
-- last, and on past any parenthesis opened in between.
spanOf :: Pg.Node -> R (Maybe (Int, Int))
spanOf n = case locsNode n of
    [] -> pure Nothing
    ls -> do
        s <- tokAt (minimum ls)
        e <- tokAt (maximum ls)
        texts <- traverse tokL [s .. e]
        let depth = length (filter (`elem` ["(", "["]) texts) - length (filter (`elem` [")", "]"]) texts)
            close i d
                | d <= 0 = pure (i - 1)
                | otherwise = do
                    t <- tokM i
                    case (.text) <$> t of
                        Nothing -> pure (i - 1)
                        Just w
                            | w `elem` ["(", "["] -> close (i + 1) (d + 1)
                            | w `elem` [")", "]"] -> close (i + 1) (d - 1)
                            | otherwise -> close (i + 1) d
        end <- close (e + 1) depth
        pure (Just (s, end))


-- | Whether an expression has a comment inside it.
hasCmts :: Pg.Node -> R Bool
hasCmts n = do
    any' <- anyComments
    if not any'
        then pure False
        else do
            sp <- spanOf n
            maybe (pure False) (uncurry commentsBetween) sp


-- | The first token of each item of a comma-separated list between two
-- tokens (a parenthesis pair), if the commas there are the list's.
listFirsts :: Int -> Int -> Int -> R (Maybe [Int])
listFirsts open close n = do
    texts <- traverse tokL [open + 1 .. close - 1]
    let commas = go (0 :: Int) (zip [open + 1 ..] texts)
        go _ [] = []
        go d ((i, w) : rest)
            | w `elem` ["(", "["] = go (d + 1) rest
            | w `elem` [")", "]"] = go (d - 1) rest
            | w == "," && d == 0 = i : go d rest
            | otherwise = go d rest
    pure (if length commas == n - 1 then Just ((open + 1) : map (+ 1) commas) else Nothing)


-- | The bracket that closes the one at a token.
closeOf :: Int -> R (Maybe Int)
closeOf open = go (open + 1) (0 :: Int)
  where
    go i d = do
        t <- tokM i
        case (.text) <$> t of
            Nothing -> pure Nothing
            Just w
                | w `elem` ["(", "["] -> go (i + 1) (d + 1)
                | w `elem` [")", "]"] -> if d == 0 then pure (Just i) else go (i + 1) (d - 1)
                | otherwise -> go (i + 1) d


-- | The first token of each item of a bracketed list, and its closing
-- bracket, when it has comments in it.
commentedList :: Int -> Int -> R (Maybe ([Int], Int))
commentedList open n = do
    mclose <- closeOf open
    case mclose of
        Nothing -> pure Nothing
        Just close -> do
            commented <- commentsBetween open close
            if not commented
                then pure Nothing
                else fmap (,close) <$> listFirsts open close n


-- | The words before a token, back over opening parentheses: where the
-- first of them is, if they are these.
wordsBefore :: [Text] -> Int -> R (Maybe Int)
wordsBefore ws i = do
    j <- back (i - 1)
    let from = j - length ws + 1
    actual <- traverse tokL [from .. j]
    pure (if from >= 0 && actual == ws then Just from else Nothing)
  where
    back j = do
        p <- tokIs j "("
        if p then back (j - 1) else pure j


-- | A clause's first token, from the keywords that start it and the node
-- that follows them.
clauseAt :: [[Text]] -> Pg.Node -> R (Maybe Int)
clauseAt alternatives n = case locsNode n of
    [] -> pure Nothing
    ls -> do
        i <- tokAt (minimum ls)
        firstJustM (`wordsBefore` i) alternatives
