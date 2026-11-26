-- | The layout's monad: the statement's source and tokens, what is placed so
-- far, and the small helpers every part of the layout uses.
module PgFmt.Layout.Monad where

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State.Strict (StateT, gets, modify')
import Control.Monad.Except (MonadError (throwError))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper, toLower)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((^.))
import PgQuery qualified as Pg
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), hcat, layoutPretty, pretty, punctuate)
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import Prettyprinter.Render.Text (renderStrict)
import PgFmt.Layout.Locations


isComment :: Pg.ScanToken -> Bool
isComment t = t ^. Pg.token `elem` [Pg.SQL_COMMENT, Pg.C_COMMENT]


layoutOpts :: LayoutOptions
layoutOpts = LayoutOptions Unbounded


indentSize :: Int
indentSize = 4


----------------------------------------------------------------------
-- The renderer's monad: the source and its tokens, and which parentheses
-- are already placed.
----------------------------------------------------------------------

data Tok = Tok
    { start :: Int
    , text :: Text
    , kind :: Pg.Token
    , isKeyword :: Bool
    , reserved :: Bool
    -- ^ A reserved keyword, which an identifier can only be quoted.
    }


data Env = Env
    { toks :: IM.IntMap Tok
    -- ^ By index.
    , byStart :: IM.IntMap Int
    -- ^ Token index by byte offset.
    , matching :: IM.IntMap Int
    -- ^ The other parenthesis of each parenthesis, by token index.
    , allLocs :: IS.IntSet
    -- ^ Every location in the statement's tree.
    , upper :: Bool
    , names :: Map.Map Text (Bool, Bool)
    -- ^ Each identifier of the source, case-folded as PostgreSQL does: whether
    -- it is written quoted, and whether unquoted.
    , cmts :: IM.IntMap [Cmt]
    -- ^ The comments after each token (by index), in order.
    , column :: Int
    -- ^ The column the statement starts at, which its lines are indented
    -- to.
    , tightTypes :: Bool
    -- ^ Types written with no space before their modifiers, @numeric(10, 2)@,
    -- as in DDL.
    , blankLineBefore :: IS.IntSet
    -- ^ The tokens (by index) with a blank line directly before them, after
    -- the token or comment that precedes them.
    }


-- | A comment, as written.
data Cmt = Cmt
    { ctext :: Text
    , ownLine :: Bool
    -- ^ Nothing but whitespace before it on its line.
    , blankBefore :: Bool
    -- ^ A blank line before it.
    }


data St = St
    { consumed :: IS.IntSet
    -- ^ Opening parentheses placed so far, by token index.
    , parenCounts :: Map.Map (ParenQuestion, NodeSpan) Int
    -- ^ How many parentheses wrap a node, once worked out.
    , boolGroups :: IS.IntSet
    -- ^ Opening parentheses placed around a group of a boolean run's
    -- arguments, which laying the run out again finds again.
    }


-- | What is asked of a node's parentheses: those around it as an
-- expression, as an indirection (whose own come before its argument's), as
-- a select, or as a join.
data ParenQuestion = AroundExpr | AroundIndirection | AroundSelect | AroundJoin
    deriving stock (Eq, Ord)


-- | A node, by its locations: the first, the last, and how many.
data NodeSpan = NodeSpan {firstLoc :: Int, lastLoc :: Int, locCount :: Int}
    deriving stock (Eq, Ord)


-- | Work out once how many parentheses wrap the node with these (non-empty)
-- locations: placing parentheses must give the same answer when asked
-- again.
remembered :: ParenQuestion -> [Int] -> R Int -> R Int
remembered question ls compute = do
    let key = (question, NodeSpan{firstLoc = minimum ls, lastLoc = maximum ls, locCount = length ls})
    known <- gets (Map.lookup key . (.parenCounts))
    case known of
        Just k -> pure k
        Nothing -> do
            k <- compute
            modify' \st -> st{parenCounts = Map.insert key k st.parenCounts}
            pure k


type R = ReaderT Env (StateT St (Either Text))


type D = Doc ()


unsupported :: Text -> R a
unsupported = throwError


mkEnv :: Bool -> ByteString -> Pg.ScanResult -> Pg.Node -> Env
mkEnv upper sql scan stmt =
    Env
        { toks = IM.fromList (zip [0 ..] ts)
        , byStart = IM.fromList (zip (map (.start) ts) [0 ..])
        , matching = IM.fromList (pairs [] (zip [0 ..] ts))
        , allLocs = IS.fromList (locsNode stmt)
        , upper
        , names = Map.fromListWith (\(a, b) (c, d) -> (a || c, b || d)) (mapMaybe' nameOf ts)
        , cmts = IM.fromListWith (flip (++)) (comments (-1) 0 (scan ^. Pg.tokens))
        , column = 0
        , tightTypes = False
        , blankLineBefore = IS.fromList (blanks 0 Nothing (scan ^. Pg.tokens))
        }
  where
    slice a b = BS.take (b - a) (BS.drop a sql)
    -- Each comment with the index of the token before it, and whether a line
    -- break separates it from what precedes it.
    -- The index of each token with a blank line before it.
    blanks _ _ [] = []
    blanks i prevEnd (t : rest)
        | isComment t = blanks i (Just e) rest
        | otherwise = [i | Just pe <- [prevEnd], BS.count 10 (slice pe s) >= 2] ++ blanks (i + 1) (Just e) rest
      where
        s = fromIntegral (t ^. Pg.start)
        e = fromIntegral (t ^. Pg.end)
    comments _ _ [] = []
    comments prev prevEnd (t : rest)
        | isComment t =
            (prev, [Cmt{ctext = TE.decodeUtf8Lenient (slice s e), ownLine = prev < 0 || BS.elem 10 (slice prevEnd s), blankBefore = BS.count 10 (slice prevEnd s) >= 2}])
                : comments prev e rest
        | otherwise = comments (prev + 1) e rest
      where
        s = fromIntegral (t ^. Pg.start)
        e = fromIntegral (t ^. Pg.end)
    ts =
        [ Tok
            { start = s
            , text = TE.decodeUtf8Lenient (BS.take (e - s) (BS.drop s sql))
            , kind = t ^. Pg.token
            , isKeyword = t ^. Pg.keywordKind /= Pg.NO_KEYWORD
            , reserved = t ^. Pg.keywordKind == Pg.RESERVED_KEYWORD
            }
        | t <- scan ^. Pg.tokens
        , not (isComment t)
        , let s = fromIntegral (t ^. Pg.start)
        , let e = fromIntegral (t ^. Pg.end)
        ]
    pairs _ [] = []
    pairs stack ((i, t) : rest)
        | t.text == "(" = pairs (i : stack) rest
        | t.text == ")", o : stack' <- stack = (o, i) : (i, o) : pairs stack' rest
        | otherwise = pairs stack rest
    nameOf t
        | t.kind == Pg.IDENT, Just quoted <- T.stripPrefix "\"" t.text =
            Just (T.replace "\"\"" "\"" (T.dropEnd 1 quoted), (True, False))
        | t.kind == Pg.IDENT || (t.isKeyword && not t.reserved) = Just (foldCase t.text, (False, True))
        | otherwise = Nothing
    mapMaybe' f = foldr (\x acc -> maybe acc (: acc) (f x)) []


-- | PostgreSQL folds unquoted identifiers' ASCII letters to lower case.
foldCase :: Text -> Text
foldCase = T.map (\c -> if isAsciiUpper c then toLower c else c)


tokM :: Int -> R (Maybe Tok)
tokM i = asks (IM.lookup i . (.toks))


-- | The token's text, lower-cased; empty past either end.
tokL :: Int -> R Text
tokL i = maybe "" (T.toLower . (.text)) <$> tokM i


tokIs :: Int -> Text -> R Bool
tokIs i t = (== t) <$> tokL i


-- | The index of the token at a byte offset.
tokAt :: Int -> R Int
tokAt loc = do
    m <- asks (.byStart)
    case IM.lookupLE loc m of
        Just (_, i) -> pure i
        Nothing -> unsupported "a location before the statement"


matchOf :: Int -> R (Maybe Int)
matchOf i = asks (IM.lookup i . (.matching))


consume :: Int -> R ()
consume i = modify' \s -> s{consumed = IS.insert i s.consumed}


isConsumed :: Int -> R Bool
isConsumed i = gets (IS.member i . (.consumed))


-- | Place the opening parenthesis at this token, which a construct's syntax
-- has there; its closing one's index.
expectParen :: Int -> R Int
expectParen i = do
    isParen <- tokIs i "("
    m <- matchOf i
    case m of
        Just close | isParen -> consume i >> pure close
        _ -> unsupported "a construct without the parenthesis it needs"


-- | The first opening parenthesis at or after this token.
nextParen :: Int -> R Int
nextParen i = do
    t <- tokM i
    case t of
        Nothing -> unsupported "no parenthesis where one is needed"
        Just tk | tk.text == "(" -> pure i
        _ -> nextParen (i + 1)


-- | Whether every location strictly between two tokens is one of these.
covers :: Int -> Int -> IS.IntSet -> R Bool
covers p m ls = do
    a <- maybe 0 (.start) <$> tokM p
    b <- maybe maxBound (.start) <$> tokM m
    every <- asks (.allLocs)
    let inside = fst (IS.split b (snd (IS.split a every)))
    pure (inside `IS.isSubsetOf` ls)


-- | Whether every one of these locations is strictly between two tokens.
allInside :: Int -> Int -> [Int] -> R Bool
allInside p m ls = do
    a <- maybe 0 (.start) <$> tokM p
    b <- maybe maxBound (.start) <$> tokM m
    pure (not (null ls) && all (\l -> l > a && l < b) ls)


kw :: Text -> R D
kw t = do
    u <- asks (.upper)
    pure (pretty (if u then T.toUpper t else t))


txt :: Text -> R D
txt = pure . pretty


-- | Text that may hold line breaks (a string literal), written as it is:
-- 'pretty' would indent its later lines with the layout.
raw :: Text -> D
raw = unsafeTextWithoutNewlines


commaJoin :: [D] -> D
commaJoin = hcat . punctuate ", "


spaceJoin :: [D] -> D
spaceJoin = hcat . punctuate " "


renderToText :: D -> Text
renderToText = renderStrict . layoutPretty layoutOpts


len :: D -> Int
len = T.length . renderToText




----------------------------------------------------------------------
-- Nodes.
----------------------------------------------------------------------

node :: Pg.Node -> Maybe Pg.Node'Node
node n = n ^. Pg.maybe'node


strOf :: Pg.Node -> Maybe Text
strOf n = case node n of
    Just (Pg.Node'String s) -> Just (s ^. Pg.sval)
    _ -> Nothing


strsOf :: [Pg.Node] -> R [Text]
strsOf = traverse \n -> maybe (unsupported "a name that is not a string") pure (strOf n)


nodeName :: Pg.Node -> Text
nodeName n = case node n of
    Nothing -> "an empty node"
    Just x -> T.takeWhile (/= ' ') (T.drop 6 (T.pack (show x)))



firstJustM :: (a -> R (Maybe b)) -> [a] -> R (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)


anyM :: (a -> R Bool) -> [a] -> R Bool
anyM _ [] = pure False
anyM f (x : xs) = f x >>= \b -> if b then pure True else anyM f xs


nonEmptyLocs :: [Int] -> R [Int]
nonEmptyLocs ls = if null ls then unsupported "a node without a location" else pure ls
