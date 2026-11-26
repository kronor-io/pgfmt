{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | pgfmt's own layout for DML statements (SELECT, INSERT, UPDATE, DELETE,
-- VALUES), rendered from libpg_query's parse tree; and for CREATE TABLE,
-- CREATE VIEW, CREATE ... AS, CREATE INDEX and ALTER TABLE, in a style of
-- its own (see the DDL section at the end).
--
-- This is the layout "PgFmt.Render" gives over postgresql-syntax's AST, and
-- it has to stay byte for byte the same: the rules (leading commas, one select
-- target per line when there are several, @join@ on its own line with @on@
-- under it, the first @where@ condition inline with hanging @and@s, ...) and
-- the thresholds are "PgFmt.Render"'s, function for function. Where
-- "PgFmt.Render" hands a node to postgresql-syntax's own renderer (and
-- lower-cases the result), this module writes that renderer's spelling too
-- (the 'P' mode below). It departs from "PgFmt.Render" to keep what the
-- source wrote (dollar-quoted strings, multi-line strings and numbers stay as
-- they are, comments are laid out rather than refused, and a blank line
-- between @values@ rows is kept), and in one rule:
-- @values@ rows that all fit go on one line each.
--
-- PostgreSQL's parse tree forgets some of what the source said that
-- postgresql-syntax's AST keeps: redundant parentheses, @!=@ for @<>@,
-- whether an alias had @as@, @inner join@ for @join@, how a type was spelled
-- (@int@, @integer@ and @int4@ are all @pg_catalog.int4@). Those come from
-- the source tokens, found through the tree's locations. Every parenthesis in
-- the source must be accounted for, as part of a construct's syntax or as
-- parentheses around one node; a statement with one that is not is refused,
-- as is any node this module does not know. Refused statements are left to
-- the caller (libpg_query's own printer, or as they are), never guessed at.
--
-- Comments are written at the line breaks the layout makes where they are
-- in the source (see 'brk'); a construct with a comment inside takes its
-- multi-line form, and a statement with a comment at no line break is
-- refused.
--
-- Keywords are written in the requested case as they are rendered;
-- identifiers, quoted or not, are never changed.
module PgFmt.Layout (
    formatStatement,
) where

import Control.Monad (forM, forM_, unless, when, zipWithM)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.ByteString (ByteString)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (isSuffixOf, zip4)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.ProtoLens.Message (FieldAccessor (..), FieldDescriptor (..), FieldTypeDescriptor (..), Message (..), ScalarField (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((&), (.~), (^.))
import PgFmt.LibPgQuery qualified as L
import PgQuery qualified as Pg
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), align, hardline, hcat, indent, layoutPretty, nest, pretty, punctuate, vsep, (<+>))
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import Prettyprinter.Render.Text (renderStrict)
import Unsafe.Coerce (unsafeCoerce)


-- | Lay out one DML statement or one of the DDL statements it knows (no comments
-- before or after it, no semicolon), keywords in upper case if asked,
-- starting at column 0 without a trailing newline. 'Left' says why not: the
-- statement is none of those, or has
-- something this renderer does not handle (a comment it cannot place among
-- them).
formatStatement :: Bool -> ByteString -> IO (Either Text Text)
formatStatement upperKeywords sql = do
    parsed <- L.parse sql
    scanned <- L.scan sql
    let laid = render parsed scanned
    case laid of
        Right out | Right scan <- scanned, hasComments scan -> do
            -- Every comment must come out once, in its order: each is
            -- written where the layout breaks a line at its place in the
            -- source, and one with no such break is not written at all.
            again <- L.scan (TE.encodeUtf8 out)
            pure case again of
                Right scan' | commentTexts (TE.encodeUtf8 out) scan' == commentTexts sql scan -> laid
                _ -> Left "a comment pgfmt cannot place"
        _ -> pure laid
  where
    render parsed scanned = case (parsed, scanned) of
        (Left err, _) -> Left err.message
        (_, Left err) -> Left err.message
        (Right tree, Right scan) -> case tree ^. Pg.stmts of
            [rawStmt] | Just stmt <- rawStmt ^. Pg.maybe'stmt -> do
                let env = mkEnv upperKeywords sql scan stmt
                (doc, st) <- runStateT (runReaderT (statement stmt) env) St{consumed = IS.empty, memo = Map.empty}
                let unplaced = [i | (i, t) <- IM.toList env.toks, t.text == "(", not (IS.member i st.consumed)]
                unless (null unplaced) (Left "a parenthesis pgfmt cannot place")
                Right (renderStrict (layoutPretty layoutOpts doc))
            _ -> Left "not a single statement"
    hasComments scan = any isComment (scan ^. Pg.tokens)
    commentTexts src scan =
        [ BS.take (fromIntegral (t ^. Pg.end) - fromIntegral (t ^. Pg.start)) (BS.drop (fromIntegral (t ^. Pg.start)) src)
        | t <- scan ^. Pg.tokens
        , isComment t
        ]


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
    , memo :: Map.Map (Int, Int, Int, Int) Int
    -- ^ How many parentheses wrap a node, once worked out.
    }


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
-- Comments: each is written at the line break the layout makes where it
-- is in the source. One on a line with code ends the line before the break;
-- one on a line of its own gets a line of its own after it. A construct
-- with a comment inside it takes its multi-line form. A comment with no
-- break at its place is not written, and the statement is refused.
----------------------------------------------------------------------

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


-- | What a check finds, or 'Nothing' where it fails.
attempt :: R a -> R (Maybe a)
attempt m = (Just <$> m) `catchError` \_ -> pure Nothing


----------------------------------------------------------------------
-- Locations.
----------------------------------------------------------------------

-- | Every location in a node's subtree.
locsNode :: Pg.Node -> [Int]
locsNode n = case n ^. Pg.maybe'node of
    Nothing -> []
    Just x -> case x of
        Pg.Node'AExpr v -> locsMsg v
        Pg.Node'ColumnRef v -> locsMsg v
        Pg.Node'ParamRef v -> locsMsg v
        Pg.Node'AConst v -> locsMsg v
        Pg.Node'FuncCall v -> locsMsg v
        Pg.Node'TypeCast v -> locsMsg v
        Pg.Node'BoolExpr v -> locsMsg v
        Pg.Node'NullTest v -> locsMsg v
        Pg.Node'BooleanTest v -> locsMsg v
        Pg.Node'SubLink v -> locsMsg v
        Pg.Node'CaseExpr v -> locsMsg v
        Pg.Node'CaseWhen v -> locsMsg v
        Pg.Node'CoalesceExpr v -> locsMsg v
        Pg.Node'MinMaxExpr v -> locsMsg v
        Pg.Node'RowExpr v -> locsMsg v
        Pg.Node'AArrayExpr v -> locsMsg v
        Pg.Node'AIndirection v -> locsMsg v
        Pg.Node'AIndices v -> locsMsg v
        Pg.Node'AStar v -> locsMsg v
        Pg.Node'CollateClause v -> locsMsg v
        Pg.Node'SqlvalueFunction v -> locsMsg v
        Pg.Node'SetToDefault v -> locsMsg v
        Pg.Node'NamedArgExpr v -> locsMsg v
        Pg.Node'GroupingFunc v -> locsMsg v
        Pg.Node'CurrentOfExpr v -> locsMsg v
        Pg.Node'List v -> locsMsg v
        Pg.Node'String v -> locsMsg v
        Pg.Node'Integer v -> locsMsg v
        Pg.Node'ResTarget v -> locsMsg v
        Pg.Node'MultiAssignRef v -> locsMsg v
        Pg.Node'SortBy v -> locsMsg v
        Pg.Node'WindowDef v -> locsMsg v
        Pg.Node'RangeVar v -> locsMsg v
        Pg.Node'RangeTableSample v -> locsMsg v
        Pg.Node'RangeSubselect v -> locsMsg v
        Pg.Node'RangeFunction v -> locsMsg v
        Pg.Node'JoinExpr v -> locsMsg v
        Pg.Node'TypeName v -> locsMsg v
        Pg.Node'ColumnDef v -> locsMsg v
        Pg.Node'IndexElem v -> locsMsg v
        Pg.Node'GroupingSet v -> locsMsg v
        Pg.Node'LockingClause v -> locsMsg v
        Pg.Node'WithClause v -> locsMsg v
        Pg.Node'CommonTableExpr v -> locsMsg v
        Pg.Node'InferClause v -> locsMsg v
        Pg.Node'OnConflictClause v -> locsMsg v
        Pg.Node'SelectStmt v -> locsMsg v
        Pg.Node'InsertStmt v -> locsMsg v
        Pg.Node'UpdateStmt v -> locsMsg v
        Pg.Node'DeleteStmt v -> locsMsg v
        Pg.Node'ReturningClause v -> locsMsg v
        _ -> locsMsg n


-- | Every location in a message's subtree, through protobuf reflection.
locsMsg :: forall m. (Message m) => m -> [Int]
locsMsg m = concatMap field (Map.elems (fieldsByTag @m))
  where
    field :: FieldDescriptor m -> [Int]
    field (FieldDescriptor name ty acc) = case ty of
        ScalarField Int32Field
            | isLocation name -> case acc of
                PlainField _ l -> located (m ^. l)
                OptionalField l -> maybe [] located (m ^. l)
                _ -> []
        MessageField _ -> case acc of
            PlainField _ l -> locsAny (m ^. l)
            OptionalField l -> maybe [] locsAny (m ^. l)
            RepeatedField _ l -> concatMap locsAny (m ^. l)
            _ -> []
        _ -> []
    located v = [fromIntegral v | v >= 0]
    isLocation name = name == "location" || "_location" `isSuffixOf` name


locsAny :: forall v. (Message v) => v -> [Int]
locsAny v
    -- The node wrapper has a field for each of its 250-odd kinds; going
    -- through 'locsNode' visits only the one it holds.
    | messageName (Proxy @v) == "pg_query.Node" = locsNode (unsafeCoerce v)
    | otherwise = locsMsg v


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


-- | An expression where it occurs: its node, the parentheses the source has
-- around it, and its parent expression (for telling whose parentheses are
-- whose).
data E = E
    { en :: Pg.Node
    , lv :: Int
    , parent :: Maybe Pg.Node
    }


-- | A child expression of an expression.
child :: Pg.Node -> Pg.Node -> R E
child p n = mkE (Just p) n


-- | An expression with no expression around it.
top :: Pg.Node -> R E
top = mkE Nothing


mkE :: Maybe Pg.Node -> Pg.Node -> R E
mkE parent n = do
    let ls = locsNode n
    lv <-
        if null ls
            then pure 0
            else do
                let key = (tag, minimum ls, maximum ls, length ls)
                known <- gets (Map.lookup key . (.memo))
                case known of
                    Just k -> pure k
                    Nothing -> do
                        k <- case node n of
                            Just (Pg.Node'AIndirection ind) -> indirectionLevels ind (IS.fromList ls)
                            _ -> do
                                s0 <- tokAt (minimum ls)
                                e0 <- tokAt (maximum ls)
                                levels s0 e0 (IS.fromList ls) (parentTakes parent)
                        modify' \s -> s{memo = Map.insert key k s.memo}
                        pure k
    pure E{en = n, lv, parent}
  where
    tag = case node n of
        Just (Pg.Node'AIndirection _) -> 1
        _ -> 0


-- | Whether a parenthesis pair is the parent expression's rather than the
-- child's: the parent lies wholly inside it.
parentTakes :: Maybe Pg.Node -> Int -> Int -> R Bool
parentTakes parent p m = case parent of
    Nothing -> pure False
    Just pn -> case node pn of
        -- An indirection's own parentheses are placed before its argument's.
        Just (Pg.Node'AIndirection _) -> pure False
        _ -> allInside p m (locsNode pn)


-- | How many parenthesis pairs wrap exactly the node spanning these tokens
-- (from its first located token to its last), placing them.
levels :: Int -> Int -> IS.IntSet -> (Int -> Int -> R Bool) -> R Int
levels s0 e0 ls takes = do
    s <- extendLeft s0
    go s e0 0
  where
    -- Parentheses inside the node that open before its first located token,
    -- as in @(a) + b@.
    extendLeft s = do
        isParen <- tokIs (s - 1) "("
        m <- matchOf (s - 1)
        case m of
            Just close | isParen, close <= e0 -> extendLeft (s - 1)
            _ -> pure s
    go s e n = do
        let p = s - 1
        isParen <- tokIs p "("
        placed <- isConsumed p
        mm <- matchOf p
        case mm of
            Just m | isParen, not placed, m > e -> do
                cov <- covers p m ls
                theirs <- takes p m
                if cov && not theirs
                    then consume p >> go p m (n + 1)
                    else pure n
            _ -> pure n


-- | An indirection's parentheses: @((a).b)@ has one around the indirection
-- and one around its argument; work out where the argument ends.
indirectionLevels :: Pg.A_Indirection -> IS.IntSet -> R Int
indirectionLevels ind ls = do
    arg <- maybe (unsupported "an indirection without an argument") pure (ind ^. Pg.maybe'arg)
    let als = locsNode arg
    when (null als) (unsupported "an indirection without a location")
    s0 <- tokAt (minimum als)
    (start, argEnd) <- case node arg of
        Just (Pg.Node'ParamRef _) -> pure (s0, s0)
        Just (Pg.Node'ColumnRef c) -> pure (s0, s0 + 2 * length (c ^. Pg.fields) - 2)
        _ -> do
            -- The outermost of the parentheses before the argument whose
            -- closing one the indirection follows.
            let walk p best = do
                    isParen <- tokIs p "("
                    if not isParen
                        then pure best
                        else do
                            m <- matchOf p
                            next <- maybe (pure "") (\c -> tokL (c + 1)) m
                            walk (p - 1) (if next `elem` [".", "["] then (p, fromMaybe p m) <$ m else best)
            found <- walk (s0 - 1) Nothing
            case node arg of
                Just (Pg.Node'SubLink sl) | isNothing found -> do
                    o <- tokAt (fromIntegral (sl ^. Pg.location))
                    c <- matchOf o
                    maybe (unsupported "a subquery without parentheses") (\c' -> pure (o, c')) c
                _ -> maybe (unsupported "an indirection on an expression without parentheses") pure found
    end <- indirectionEnd (argEnd + 1) (ind ^. Pg.indirection)
    levels start end ls (\_ _ -> pure False)


-- | The last token of an indirection starting at this token.
indirectionEnd :: Int -> [Pg.Node] -> R Int
indirectionEnd i [] = pure (i - 1)
indirectionEnd i (el : rest) = case node el of
    Just (Pg.Node'AIndices _) -> do
        isBracket <- tokIs i "["
        unless isBracket (unsupported "a subscript without brackets")
        close <- bracketEnd (i + 1) (0 :: Int)
        indirectionEnd (close + 1) rest
    _ -> indirectionEnd (i + 2) rest
  where
    bracketEnd j depth = do
        t <- tokM j
        case (.text) <$> t of
            Nothing -> unsupported "an unclosed subscript"
            Just "]" | depth == 0 -> pure j
            Just "]" -> bracketEnd (j + 1) (depth - 1)
            Just "[" -> bracketEnd (j + 1) (depth + 1)
            _ -> bracketEnd (j + 1) depth


----------------------------------------------------------------------
-- Identifiers, constants, types.
----------------------------------------------------------------------

-- | An identifier as the source writes it, quoted or not.
ident :: Text -> R D
ident name = do
    ns <- asks (.names)
    let plain = case T.uncons name of
            Just (c, rest) -> (isAsciiLower c || c == '_') && T.all (\x -> isAsciiLower x || isDigit x || x == '_' || x == '$') rest
            Nothing -> False
    when (T.any (== '"') name) (unsupported "an identifier with a double quote in it")
    case Map.lookup name ns of
        Just (True, False) -> txt ("\"" <> name <> "\"")
        Just (False, True) | plain -> txt name
        Just (True, True) -> unsupported ("identifier " <> name <> " written both quoted and unquoted")
        _ | not plain -> unsupported ("identifier " <> name <> " not found quoted")
        _ -> unsupported ("identifier " <> name <> " not found in the source")


-- | A dotted name.
qualName :: [Text] -> R D
qualName parts = hcat . punctuate "." <$> traverse ident parts


-- | An identifier at a known token: written as that token writes it, if it
-- spells the identifier.
identAt :: Int -> Text -> R D
identAt i name = do
    t <- tokM i
    case t of
        Just tk | identText tk == Just name -> txt (if "\"" `T.isPrefixOf` tk.text then "\"" <> name <> "\"" else name)
        -- PostgreSQL cuts a name longer than 63 bytes to 63.
        Just tk
            | Just full <- identText tk
            , not ("\"" `T.isPrefixOf` tk.text)
            , T.all isAscii full
            , T.length full > 63
            , T.take 63 full == name ->
                txt full
        _ -> ident name


-- | A dotted name starting at a known token.
qualNameAt :: Int -> [Text] -> R D
qualNameAt i parts = hcat . punctuate "." <$> zipWithM (\k p -> identAt (i + 2 * k) p) [0 ..] parts


-- | Rendering modes for expressions. 'MR' is "PgFmt.Render"'s own
-- @renderExpr@; 'MP' is postgresql-syntax's renderer, lower-cased, which
-- "PgFmt.Render" uses for the nodes it does not lay out itself; 'MB' is
-- "PgFmt.Render"'s @renderBExpr@, for the lower bound of a @between@.
data Mode = MR | MB | MP
    deriving stock (Eq)


aConst :: Mode -> Pg.A_Const -> R D
aConst mode c = do
    i <- tokAt (fromIntegral (c ^. Pg.location))
    t <- tokM i
    let source = maybe "" (.text) t
    case c ^. Pg.maybe'val of
        _ | c ^. Pg.isnull -> kw "null"
        Nothing -> kw "null"
        Just (Pg.A_Const'Ival v) -> number (fromIntegral (v ^. Pg.ival) :: Integer) i source
        Just (Pg.A_Const'Fval v) -> do
            let s = v ^. Pg.fval
            case T.stripPrefix "-" s of
                Just _ | source == "-" -> do
                    digits <- tokL (i + 1)
                    pure (minus <> pretty (normalNumber digits))
                _ -> pure (pretty (normalNumber (T.toLower source)))
        Just (Pg.A_Const'Boolval v) -> kw (if v ^. Pg.boolval then "true" else "false")
        Just (Pg.A_Const'Sval v) -> stringConst (v ^. Pg.sval) source
        Just (Pg.A_Const'Bsval v) -> do
            let b = v ^. Pg.bsval
            pure (pretty (T.toLower (T.take 1 b)) <> "'" <> pretty (T.drop 1 b) <> "'")
  where
    minus = if mode == MP then "- " else "-"
    number v i source
        | v < 0, source == "-" = do
            digits <- tokL (i + 1)
            pure (minus <> pretty (if T.all isDigit digits then T.pack (show (negate v)) else digits))
        | T.all isDigit source = txt (T.pack (show v))
        | otherwise = txt (T.toLower source)
    -- postgresql-syntax reads integers that do not fit an int4 as integers
    -- too, without leading zeros.
    normalNumber s
        | not (T.null s), T.all isDigit s = T.pack (show (read (T.unpack s) :: Integer))
        | otherwise = s


-- | A string constant: a plain one as postgresql-syntax writes it (which
-- joins a string continued over lines into one); any other kind
-- (dollar-quoted, @E'...'@) as written.
stringConst :: Text -> Text -> R D
stringConst value source
    | "'" `T.isPrefixOf` source = pure (raw ("'" <> T.replace "'" "''" value <> "'"))
    | otherwise = pure (raw source)


-- | A type name, as postgresql-syntax spells it: SQL's own type names are
-- keywords, written with a space before their modifiers (@numeric (10, 2)@);
-- any other is a name.
typeName :: Pg.TypeName -> R D
typeName tn = do
    when (tn ^. Pg.setof || tn ^. Pg.pctType) (unsupported "setof or %type")
    i <- tokAt (fromIntegral (tn ^. Pg.location))
    (d, next) <- typeWords False i
    arr <- arrayBounds next (length (tn ^. Pg.arrayBounds))
    pure (d <> arr)


-- | The words of a type starting at this token, and the token after them.
-- In a typed literal (@date '...'@), a type that is not one of SQL's has no
-- space before its modifiers.
typeWords :: Bool -> Int -> R (D, Int)
typeWords literal i = do
    w <- tokL i
    w1 <- tokL (i + 1)
    w2 <- tokL (i + 2)
    let fixed n words' = do
            d <- kw (T.unwords words')
            pure (d, i + n)
    case w of
        "interval" -> do
            -- @interval (p)@
            isParen <- tokIs (i + 1) "("
            if isParen && not literal
                then do
                    (args, next) <- modifiers (i + 1)
                    d <- kw "interval"
                    sep <- modifierSep
                    pure (d <> sep <> args, next)
                else do
                    d <- kw "interval"
                    (fields, next) <- intervalFields (i + 1)
                    pure (maybe d (\f -> d <> " " <> f) fields, next)
        _ | w `elem` ["timestamp", "time"] -> do
            d <- kw w
            isParen <- tokIs (i + 1) "("
            (precision, j) <- if isParen then (\(a, n) -> (Just a, n)) <$> modifiers (i + 1) else pure (Nothing, i + 1)
            a <- tokL j
            b <- tokL (j + 1)
            c <- tokL (j + 2)
            zone <-
                if a `elem` ["with", "without"] && b == "time" && c == "zone"
                    then Just <$> kw (T.unwords [a, b, c])
                    else pure Nothing
            sep <- modifierSep
            pure ((d <> maybe mempty (sep <>) precision) <> maybe mempty (" " <>) zone, if isJust zone then j + 3 else j)
        _ | w `elem` ["integer", "int", "smallint", "bigint", "real", "boolean"] -> fixed 1 [w]
        "double" | w1 == "precision" -> fixed 2 [w, w1]
        _ | w `elem` ["float", "decimal", "dec", "numeric"] -> withModifiers 1 [w]
        "bit" | w1 == "varying" -> withModifiers 2 [w, w1]
        "bit" -> withModifiers 1 [w]
        _ | w `elem` ["character", "char", "nchar"], w1 == "varying" -> withModifiers 2 [w, w1]
        _ | w `elem` ["character", "char", "nchar", "varchar"] -> withModifiers 1 [w]
        "national" | w1 `elem` ["character", "char"], w2 == "varying" -> withModifiers 3 [w, w1, w2]
        "national" | w1 `elem` ["character", "char"] -> withModifiers 2 [w, w1]
        _ -> do
            -- A name, maybe qualified, maybe with modifiers.
            (parts, j) <- dotted i
            name <- qualName parts
            isParen <- tokIs j "("
            if isParen
                then do
                    (args, next) <- modifiers j
                    sep <- modifierSep
                    pure (name <> (if literal then mempty else sep) <> args, next)
                else pure (name, j)
  where
    withModifiers n words' = do
        d <- kw (T.unwords words')
        isParen <- tokIs (i + n) "("
        if isParen
            then do
                (args, next) <- modifiers (i + n)
                sep <- modifierSep
                pure (d <> sep <> args, next)
            else pure (d, i + n)
    dotted j = do
        t <- tokM j
        case t of
            Just tk | tk.kind == Pg.IDENT || tk.isKeyword -> do
                let part = if "\"" `T.isPrefixOf` tk.text then T.replace "\"\"" "\"" (T.drop 1 (T.dropEnd 1 tk.text)) else foldCase tk.text
                isDot <- tokIs (j + 1) "."
                if isDot
                    then do
                        (rest, k) <- dotted (j + 2)
                        pure (part : rest, k)
                    else pure ([part], j + 1)
            _ -> unsupported "a type name pgfmt cannot read"


-- | What goes between a type and its modifiers.
modifierSep :: R D
modifierSep = asks \env -> if env.tightTypes then mempty else " "


-- | A type's modifiers, @(10, 2)@, from the parenthesis at this token: the
-- rendered list and the token after it.
modifiers :: Int -> R (D, Int)
modifiers i = do
    close <- expectParen i
    items <- forM [i + 1 .. close - 1] \j -> do
        t <- tokM j
        case t of
            Just tk
                | tk.text == "," -> pure ", "
                | tk.kind `elem` [Pg.ICONST, Pg.FCONST] -> pure (pretty (T.toLower tk.text))
                | tk.kind == Pg.IDENT || tk.isKeyword -> ident (if "\"" `T.isPrefixOf` tk.text then T.drop 1 (T.dropEnd 1 tk.text) else foldCase tk.text)
            _ -> unsupported "a type modifier pgfmt cannot read"
    pure ("(" <> mconcat items <> ")", close + 1)


-- | @year@, @day to second@, @second (3)@ after @interval@.
intervalFields :: Int -> R (Maybe D, Int)
intervalFields i = do
    w <- tokL i
    if w `notElem` ["year", "month", "day", "hour", "minute", "second"]
        then pure (Nothing, i)
        else do
            (first, j) <- field w i
            to <- tokL j
            if to == "to"
                then do
                    w' <- tokL (j + 1)
                    (second, k) <- field w' (j + 1)
                    toD <- kw "to"
                    pure (Just (first <+> toD <+> second), k)
                else pure (Just first, j)
  where
    field w j = do
        d <- kw w
        isParen <- tokIs (j + 1) "("
        if w == "second" && isParen
            then do
                (p, next) <- modifiers (j + 1)
                pure (d <> " " <> p, next)
            else pure (d, j + 1)


-- | Array bounds after a type: @[]@, @[3]@, or @array@, @array[3]@.
arrayBounds :: Int -> Int -> R D
arrayBounds _ 0 = pure mempty
arrayBounds i n = do
    w <- tokL i
    if w == "array"
        then do
            d <- kw "array"
            isBracket <- tokIs (i + 1) "["
            if isBracket
                then do
                    size <- tokL (i + 2)
                    pure (" " <> d <> "[" <> pretty size <> "]")
                else pure (" " <> d)
        else do
            dims <- go i n
            pure (spaceJoin dims)
  where
    go _ 0 = pure []
    go j k = do
        isBracket <- tokIs j "["
        unless isBracket (unsupported "array bounds pgfmt cannot read")
        close <- tokIs (j + 1) "]"
        if close
            then ("[]" :) <$> go (j + 2) (k - 1)
            else do
                size <- tokL (j + 1)
                ("[" <> pretty size <> "]" :) <$> go (j + 3) (k - 1)


----------------------------------------------------------------------
-- Expressions, inline.
----------------------------------------------------------------------

-- | An expression on one line: "PgFmt.Render"'s @renderExpr@ ('MR'), its
-- @renderBExpr@ ('MB'), or postgresql-syntax's renderer ('MP').
expr :: Mode -> E -> R D
expr mode e
    | e.lv > 0 = do
        -- In parentheses, an expression is a full one again.
        inner <- expr (if mode == MP then MP else MR) e{lv = e.lv - 1}
        pure ("(" <> inner <> ")")
    | otherwise = core mode e


-- | 'MB' renders its own operators and goes back to 'MR' below them, as
-- @renderBExpr@ does for anything that is a @c_expr@.
core :: Mode -> E -> R D
core mode e = case node e.en of
    Just (Pg.Node'ColumnRef c) -> columnRef c
    Just (Pg.Node'ParamRef p) -> txt ("$" <> T.pack (show (p ^. Pg.number)))
    Just (Pg.Node'AConst c) -> aConst mode c
    Just (Pg.Node'TypeCast tc) -> typeCast mode e tc
    Just (Pg.Node'AExpr a) -> aExpr mode e a
    Just (Pg.Node'BoolExpr b) -> boolExpr (sub mode) e b
    Just (Pg.Node'NullTest t) -> do
        arg <- req "a null test" (t ^. Pg.maybe'arg) >>= child e.en
        i <- tokAt (fromIntegral (t ^. Pg.location))
        w <- tokL i
        a <- expr (sub mode) arg
        test <- case (t ^. Pg.nulltesttype, w) of
            (_, "isnull") -> kw "isnull"
            (_, "notnull") -> kw "notnull"
            (Pg.IS_NULL, _) -> kw "is null"
            _ -> kw "is not null"
        pure (a <+> test)
    Just (Pg.Node'BooleanTest t) -> do
        arg <- req "a boolean test" (t ^. Pg.maybe'arg) >>= child e.en
        a <- expr (sub mode) arg
        test <- kw case t ^. Pg.booltesttype of
            Pg.IS_TRUE -> "is true"
            Pg.IS_NOT_TRUE -> "is not true"
            Pg.IS_FALSE -> "is false"
            Pg.IS_NOT_FALSE -> "is not false"
            Pg.IS_UNKNOWN -> "is unknown"
            _ -> "is not unknown"
        pure (a <+> test)
    Just (Pg.Node'FuncCall f) -> funcCall (sub mode) e f
    Just (Pg.Node'SubLink s) -> subLink (sub mode) e s
    Just (Pg.Node'CaseExpr c) -> caseInline (sub mode) e c
    Just (Pg.Node'CoalesceExpr c) -> keywordCall (sub mode) "coalesce" (c ^. Pg.location) (c ^. Pg.args) e
    Just (Pg.Node'MinMaxExpr c) ->
        keywordCall (sub mode) (if c ^. Pg.op == Pg.IS_GREATEST then "greatest" else "least") (c ^. Pg.location) (c ^. Pg.args) e
    Just (Pg.Node'RowExpr r) -> rowExpr (sub mode) e r
    Just (Pg.Node'AArrayExpr a) -> arrayExpr (sub mode) e a
    Just (Pg.Node'AIndirection ind) -> indirectionExpr (sub mode) e ind
    Just (Pg.Node'CollateClause c) -> do
        arg <- req "a collation" (c ^. Pg.maybe'arg) >>= child e.en
        let m = if mode == MB then MP else mode
        a <- expr m arg
        name <- strsOf (c ^. Pg.collname) >>= qualName
        k <- kw "collate"
        pure (a <+> k <+> name)
    Just (Pg.Node'SqlvalueFunction f) -> sqlValueFunction f
    Just (Pg.Node'SetToDefault _) -> kw "default"
    Just (Pg.Node'GroupingFunc g) -> do
        open <- tokAt (fromIntegral (g ^. Pg.location)) >>= nextParen
        _ <- expectParen open
        args <- traverse (child e.en >=> expr MP) (g ^. Pg.args)
        k <- kw "grouping"
        pure (k <> " (" <> commaJoin args <> ")")
    _ -> unsupported (nodeName e.en)
  where
    sub MB = MR
    sub m = m


(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g


req :: Text -> Maybe a -> R a
req what = maybe (unsupported (what <> " with a part missing")) pure


columnRef :: Pg.ColumnRef -> R D
columnRef c = do
    at <- tokAt (fromIntegral (c ^. Pg.location))
    parts <- forM (zip [0 ..] (c ^. Pg.fields)) \(k, f) -> case node f of
        Just (Pg.Node'String s) -> identAt (at + 2 * k) (s ^. Pg.sval)
        Just (Pg.Node'AStar _) -> txt "*"
        _ -> unsupported "a column reference pgfmt cannot read"
    pure (hcat (punctuate "." parts))


typeCast :: Mode -> E -> Pg.TypeCast -> R D
typeCast mode e tc = do
    tn <- req "a cast" (tc ^. Pg.maybe'typeName)
    arg <- req "a cast" (tc ^. Pg.maybe'arg)
    form <- castForm tc
    case form of
        TypedLiteral -> typedLiteral tc
        CastCall -> do
            argE <- child e.en arg
            open <- tokAt (fromIntegral (tc ^. Pg.location)) >>= nextParen
            _ <- expectParen open
            a <- expr (if mode == MP then MP else MR) argE
            t <- typeName tn
            k <- kw "cast"
            as <- kw "as"
            pure (k <> (if mode == MP then " (" else "(") <> a <+> as <+> t <> ")")
        ColonCast -> do
            argE <- child e.en arg
            a <- expr mode argE
            t <- typeName tn
            pure (a <> "::" <> t)


data CastForm = ColonCast | CastCall | TypedLiteral


castForm :: Pg.TypeCast -> R CastForm
castForm tc
    | tc ^. Pg.location < 0 = pure TypedLiteral
    | otherwise = do
        w <- tokAt (fromIntegral (tc ^. Pg.location)) >>= tokL
        pure if w == "cast" then CastCall else ColonCast


-- | @date '2020-01-01'@, @interval '1' day@.
typedLiteral :: Pg.TypeCast -> R D
typedLiteral tc = do
    tn <- req "a typed literal" (tc ^. Pg.maybe'typeName)
    arg <- req "a typed literal" (tc ^. Pg.maybe'arg)
    value <- case node arg of
        Just (Pg.Node'AConst c) | Just (Pg.A_Const'Sval s) <- c ^. Pg.maybe'val -> do
            i <- tokAt (fromIntegral (c ^. Pg.location))
            source <- maybe "" (.text) <$> tokM i
            (,) i <$> stringConst (s ^. Pg.sval) source
        _ -> unsupported "a typed literal that is not a string"
    unless (null (tn ^. Pg.arrayBounds)) (unsupported "a typed literal of an array type")
    i <- tokAt (fromIntegral (tn ^. Pg.location))
    w <- tokL i
    (ty, next) <- typeWords True i
    unless (next == fst value) (unsupported "a typed literal pgfmt cannot read")
    if w == "interval"
        then do
            (fields, _) <- intervalFields (fst value + 1)
            pure (ty <> " " <> snd value <> maybe mempty (" " <>) fields)
        else pure (ty <> " " <> snd value)


-- | The operator token of an operator expression, as written.
operatorText :: Pg.A_Expr -> R Text
operatorText a = do
    names <- strsOf (a ^. Pg.name)
    op <- case names of
        [op] -> pure op
        _ -> unsupported "a schema-qualified operator"
    source <- tokAt (fromIntegral (a ^. Pg.location)) >>= tokL
    if source == op || (op == "<>" && source == "!=")
        then pure source
        else unsupported "an operator pgfmt cannot find in the source"


jsonOps :: [Text]
jsonOps = ["->", "->>", "#>", "#>>"]


aExpr :: Mode -> E -> Pg.A_Expr -> R D
aExpr mode e a = case a ^. Pg.kind of
    Pg.AEXPR_OP -> do
        op <- operatorText a
        r <- req "an operator" (a ^. Pg.maybe'rexpr) >>= child e.en
        case a ^. Pg.maybe'lexpr of
            Nothing
                | op `elem` ["-", "+"] -> do
                    rd <- expr mode r
                    pure (pretty op <> (if mode == MP then " " else mempty) <> rd)
                | otherwise -> do
                    -- Any other prefix operator: @~ x@.
                    rd <- expr (if mode == MB then MP else mode) r
                    pure (pretty op <+> rd)
            Just lexpr -> do
                l <- child e.en lexpr
                ld <- expr mode l
                rd <- expr mode r
                pure
                    if mode == MR && op `elem` jsonOps
                        then ld <> pretty op <> rd
                        else ld <+> pretty op <+> rd
    _ | mode == MB -> aExpr MP e a
    k | k `elem` [Pg.AEXPR_OP_ANY, Pg.AEXPR_OP_ALL] -> do
        op <- operatorText a
        opTok <- tokAt (fromIntegral (a ^. Pg.location))
        subType <- tokL (opTok + 1)
        unless (subType `elem` ["any", "some", "all"]) (unsupported "an any/all pgfmt cannot read")
        _ <- expectParen (opTok + 2)
        l <- req "an any/all" (a ^. Pg.maybe'lexpr) >>= child e.en
        r <- req "an any/all" (a ^. Pg.maybe'rexpr) >>= child e.en
        ld <- expr mode l
        st <- kw subType
        rd <- if mode == MP then expr MP r else exprMaybeWrap r
        pure (ld <+> pretty op <+> st <> (if mode == MP then " (" else "(") <> rd <> ")")
    k | k `elem` [Pg.AEXPR_DISTINCT, Pg.AEXPR_NOT_DISTINCT] -> do
        l <- req "a distinct" (a ^. Pg.maybe'lexpr) >>= child e.en
        r <- req "a distinct" (a ^. Pg.maybe'rexpr) >>= child e.en
        ld <- expr mode l
        rd <- expr mode r
        k' <- kw (if k == Pg.AEXPR_DISTINCT then "is distinct from" else "is not distinct from")
        pure (ld <+> k' <+> rd)
    Pg.AEXPR_NULLIF -> do
        open <- tokAt (fromIntegral (a ^. Pg.location)) >>= nextParen
        _ <- expectParen open
        l <- req "a nullif" (a ^. Pg.maybe'lexpr) >>= child e.en
        r <- req "a nullif" (a ^. Pg.maybe'rexpr) >>= child e.en
        ld <- expr mode l
        rd <- expr mode r
        k <- kw "nullif"
        pure (k <> (if mode == MP then " (" else "(") <> ld <> ", " <> rd <> ")")
    Pg.AEXPR_IN -> do
        names <- strsOf (a ^. Pg.name)
        open <- tokAt (fromIntegral (a ^. Pg.rexprListStart))
        _ <- expectParen open
        l <- req "an in" (a ^. Pg.maybe'lexpr) >>= child e.en
        items <- case a ^. Pg.maybe'rexpr >>= node of
            Just (Pg.Node'List xs) -> pure (xs ^. Pg.items)
            _ -> unsupported "an in without a list"
        ld <- expr mode l
        k <- kw (if names == ["<>"] then "not in" else "in")
        positions <- commentedList open (length items)
        case positions of
            Nothing -> do
                ds <- traverse (child e.en >=> expr MP) items
                pure (ld <+> k <+> "(" <> commaJoin ds <> ")")
            Just _ -> do
                ds <- traverse (child e.en >=> exprMaybeWrap) items
                (rows, end) <- argRowsAt positions ds
                pure (ld <+> k <+> "(" <> nest indentSize rows <> end <> ")")
    k | k `elem` [Pg.AEXPR_LIKE, Pg.AEXPR_ILIKE] -> do
        names <- strsOf (a ^. Pg.name)
        word <- case names of
            ["~~"] -> pure "like"
            ["!~~"] -> pure "not like"
            ["~~*"] -> pure "ilike"
            ["!~~*"] -> pure "not ilike"
            _ -> unsupported "a like pgfmt cannot read"
        l <- req "a like" (a ^. Pg.maybe'lexpr) >>= child e.en
        rn <- req "a like" (a ^. Pg.maybe'rexpr)
        case node rn of
            Just (Pg.Node'FuncCall _) -> unsupported "like ... escape"
            _ -> pure ()
        r <- child e.en rn
        ld <- expr mode l
        rd <- expr mode r
        k' <- kw word
        pure (ld <+> k' <+> rd)
    k | k `elem` [Pg.AEXPR_BETWEEN, Pg.AEXPR_NOT_BETWEEN, Pg.AEXPR_BETWEEN_SYM, Pg.AEXPR_NOT_BETWEEN_SYM] -> do
        i <- tokAt (fromIntegral (a ^. Pg.location))
        w <- tokL i
        w1 <- tokL (i + 1)
        let negated = k `elem` [Pg.AEXPR_NOT_BETWEEN, Pg.AEXPR_NOT_BETWEEN_SYM]
            symmetric = k `elem` [Pg.AEXPR_BETWEEN_SYM, Pg.AEXPR_NOT_BETWEEN_SYM]
            afterBetween = if w == "not" then i + 2 else i + 1
        asym <- tokIs afterBetween "asymmetric"
        when (asym || (w /= "between" && not (w == "not" && w1 == "between"))) (unsupported "a between pgfmt cannot read")
        l <- req "a between" (a ^. Pg.maybe'lexpr) >>= child e.en
        (lo, hi) <- case a ^. Pg.maybe'rexpr >>= node of
            Just (Pg.Node'List xs) | [x, y] <- xs ^. Pg.items -> (,) <$> child e.en x <*> child e.en y
            _ -> unsupported "a between without two bounds"
        ld <- expr mode l
        bk <- kw ((if negated then "not " else "") <> (if symmetric then "between symmetric" else "between"))
        andK <- kw "and"
        (lod, hid) <-
            if mode == MP || symmetric
                then (,) <$> expr MP lo <*> expr MP hi
                else (,) <$> expr MB lo <*> expr MR hi
        pure (ld <+> bk <+> lod <+> andK <+> hid)
    _ -> unsupported "an operator expression pgfmt does not lay out"


boolExpr :: Mode -> E -> Pg.BoolExpr -> R D
boolExpr mode e b = case b ^. Pg.boolop of
    Pg.NOT_EXPR -> do
        argN <- case b ^. Pg.args of
            [x] -> pure x
            _ -> unsupported "a not with several arguments"
        arg <- child e.en argN
        notAt <- tokAt (fromIntegral (b ^. Pg.location))
        argStart <- tokAt (minimum (fromIntegral (b ^. Pg.location) : locsNode argN))
        if argStart < notAt
            then -- @x not in (select ...)@
            case node argN of
                Just (Pg.Node'SubLink s) | s ^. Pg.subLinkType == Pg.ANY_SUBLINK, null (s ^. Pg.operName) -> do
                    test <- req "a not in" (s ^. Pg.maybe'testexpr) >>= child arg.en
                    inSubLink mode test s True
                _ -> unsupported "a not pgfmt cannot read"
            else do
                k <- kw "not"
                d <- expr mode arg
                pure (k <+> d)
    op -> do
        args <- traverse (child e.en) (b ^. Pg.args)
        k <- kw (if op == Pg.AND_EXPR then "and" else "or")
        boolArgs mode (fromIntegral (b ^. Pg.location)) (" " <> k <> " ") args


-- | The arguments of an @and@ or @or@. PostgreSQL flattens @(a and b) and c@
-- into one list of three (at the location of the first @and@), so
-- parentheses the source has around a run of them are placed here.
boolArgs :: Mode -> Int -> D -> [E] -> R D
boolArgs mode loc sep = boolRun mode loc sep IS.empty


-- | 'boolArgs' inside the parentheses already placed around a run.
boolRun :: Mode -> Int -> D -> IS.IntSet -> [E] -> R D
boolRun mode loc sep outer args = hcat . punctuate sep <$> go args
  where
    go [] = pure []
    go (x : rest) = do
        grouped <- group x rest
        case grouped of
            Just (p, inside, after) -> do
                d <- boolRun (if mode == MP then MP else MR) loc sep (IS.insert p outer) inside
                (("(" <> d <> ")") :) <$> go after
            Nothing -> (:) <$> expr mode x <*> go rest
    -- The longest run from this argument that a parenthesis pair encloses
    -- exactly, trying the outermost of the parentheses before it first.
    group x rest = do
        s <- argStart x
        ps <- parensBefore (s - 1)
        firstJustM (tryParen x rest) (reverse (filter (`IS.notMember` outer) ps))
    parensBefore p = do
        isParen <- tokIs p "("
        if isParen then (p :) <$> parensBefore (p - 1) else pure []
    tryParen x rest p = do
        placed <- isConsumed p
        -- Rendering again finds the same run: the pair is placed by now.
        mine <- gets (Map.member (4, p, 0, 0) . (.memo))
        mm <- matchOf p
        case mm of
            Just m | not placed || mine -> do
                let candidates = [(take k (x : rest), drop k (x : rest)) | k <- [length rest + 1, length rest .. 2]]
                found <- firstM (encloses p m . fst) candidates
                case found of
                    Just (inside, after) -> do
                        consume p
                        modify' \st -> st{memo = Map.insert (4, p, 0, 0) 1 st.memo}
                        pure (Just (p, inside, after))
                    Nothing -> pure Nothing
            _ -> pure Nothing
    encloses p m run = do
        let ls = concatMap (locsNode . (.en)) run
        inside <- allInside p m ls
        cov <- covers p m (IS.fromList (loc : ls))
        pure (inside && cov)
    argStart x = do
        ls <- nonEmptyLocs (locsNode x.en)
        i <- tokAt (minimum ls)
        e0 <- tokAt (maximum ls)
        let back j = do
                isParen <- tokIs (j - 1) "("
                m <- matchOf (j - 1)
                case m of
                    Just close | isParen, close <= e0 -> back (j - 1)
                    _ -> pure j
        j <- back i
        pure (j - x.lv)


firstJustM :: (a -> R (Maybe b)) -> [a] -> R (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)


firstM :: (a -> R Bool) -> [a] -> R (Maybe a)
firstM _ [] = pure Nothing
firstM f (x : xs) = f x >>= \b -> if b then pure (Just x) else firstM f xs


sqlValueFunction :: Pg.SQLValueFunction -> R D
sqlValueFunction f = do
    let precision = " (" <> pretty (f ^. Pg.typmod) <> ")"
    case f ^. Pg.op of
        Pg.SVFOP_CURRENT_DATE -> kw "current_date"
        Pg.SVFOP_CURRENT_TIME -> kw "current_time"
        Pg.SVFOP_CURRENT_TIME_N -> (<> precision) <$> kw "current_time"
        Pg.SVFOP_CURRENT_TIMESTAMP -> kw "current_timestamp"
        Pg.SVFOP_CURRENT_TIMESTAMP_N -> (<> precision) <$> kw "current_timestamp"
        Pg.SVFOP_LOCALTIME -> kw "localtime"
        Pg.SVFOP_LOCALTIME_N -> (<> precision) <$> kw "localtime"
        Pg.SVFOP_LOCALTIMESTAMP -> kw "localtimestamp"
        Pg.SVFOP_LOCALTIMESTAMP_N -> (<> precision) <$> kw "localtimestamp"
        Pg.SVFOP_CURRENT_ROLE -> kw "current_role"
        Pg.SVFOP_CURRENT_USER -> kw "current_user"
        Pg.SVFOP_USER -> kw "user"
        Pg.SVFOP_SESSION_USER -> kw "session_user"
        Pg.SVFOP_CURRENT_CATALOG -> kw "current_catalog"
        Pg.SVFOP_CURRENT_SCHEMA -> kw "current_schema"
        _ -> unsupported "an SQL value function pgfmt does not know"


-- | @coalesce(a, b)@, @greatest(a, b)@: in 'MP', postgresql-syntax puts a
-- space before the parenthesis.
keywordCall :: Mode -> Text -> Int32 -> [Pg.Node] -> E -> R D
keywordCall mode name loc args e = do
    open <- tokAt (fromIntegral loc) >>= nextParen
    _ <- expectParen open
    ds <- traverse (child e.en >=> expr mode) args
    k <- kw name
    pure (k <> (if mode == MP then " (" else "(") <> commaJoin ds <> ")")


rowExpr :: Mode -> E -> Pg.RowExpr -> R D
rowExpr mode e r = do
    at <- tokAt (fromIntegral (r ^. Pg.location))
    explicit <- tokIs at "row"
    open <- if explicit then nextParen at else pure at
    _ <- expectParen open
    ds <- traverse (child e.en >=> expr mode) (r ^. Pg.args)
    if explicit
        then do
            k <- kw "row"
            pure (k <> (if mode == MP then " (" else "(") <> commaJoin ds <> ")")
        else pure ("(" <> commaJoin ds <> ")")


-- | Whether an array expression has the @array@ keyword (nested ones do not).
arrayKeyword :: Pg.A_ArrayExpr -> R Bool
arrayKeyword a = tokAt (fromIntegral (a ^. Pg.location)) >>= (`tokIs` "array")


arrayExpr :: Mode -> E -> Pg.A_ArrayExpr -> R D
arrayExpr mode e a = do
    keyword <- arrayKeyword a
    commented <- hasCmts e.en
    inner <- if commented then arrayWrapped e a else arrayBody mode e a
    k <- kw "array"
    pure if keyword then k <> (if mode == MP && not commented then " " else mempty) <> inner else inner


arrayBody :: Mode -> E -> Pg.A_ArrayExpr -> R D
arrayBody mode e a = do
    ds <- traverse (child e.en >=> expr mode) (a ^. Pg.elements)
    pure ("[" <> commaJoin ds <> "]")


indirectionExpr :: Mode -> E -> Pg.A_Indirection -> R D
indirectionExpr mode e ind = do
    argN <- req "an indirection" (ind ^. Pg.maybe'arg)
    arg <- child e.en argN
    (a, m) <- case node argN of
        -- postgresql-syntax reads a subscripted column as one column
        -- reference.
        Just (Pg.Node'ColumnRef c) | arg.lv == 0 -> (,MP) <$> columnRef c
        _ -> (,mode) <$> expr mode arg
    i <- indirection m e.en (ind ^. Pg.indirection)
    pure (a <> i)


indirection :: Mode -> Pg.Node -> [Pg.Node] -> R D
indirection mode p els = mconcat <$> traverse el els
  where
    el n = case node n of
        Just (Pg.Node'String s) -> ("." <>) <$> ident (s ^. Pg.sval)
        Just (Pg.Node'AStar _) -> txt ".*"
        Just (Pg.Node'AIndices ix)
            | ix ^. Pg.isSlice -> do
                lo <- maybe (pure mempty) (child p >=> expr mode) (ix ^. Pg.maybe'lidx)
                hi <- maybe (pure mempty) (child p >=> expr mode) (ix ^. Pg.maybe'uidx)
                pure ("[" <> lo <> ":" <> hi <> "]")
            | otherwise -> do
                i <- maybe (unsupported "a subscript") (child p >=> expr mode) (ix ^. Pg.maybe'uidx)
                pure ("[" <> i <> "]")
        _ -> unsupported "an indirection pgfmt cannot read"


caseInline :: Mode -> E -> Pg.CaseExpr -> R D
caseInline mode e c = do
    k <- kw "case"
    arg <- maybe (pure mempty) (\a -> (" " <>) <$> (child e.en a >>= expr mode)) (c ^. Pg.maybe'arg)
    whens <- forM (c ^. Pg.args) \w -> case node w of
        Just (Pg.Node'CaseWhen cw) -> do
            cond <- req "a when" (cw ^. Pg.maybe'expr) >>= child e.en >>= expr mode
            result <- req "a when" (cw ^. Pg.maybe'result) >>= child e.en >>= expr mode
            whenK <- kw "when"
            thenK <- kw "then"
            pure (" " <> whenK <+> cond <+> thenK <+> result)
        _ -> unsupported "a case branch pgfmt cannot read"
    def <- case c ^. Pg.maybe'defresult of
        Nothing -> pure mempty
        Just d -> do
            elseK <- kw "else"
            dd <- child e.en d >>= expr mode
            pure (" " <> elseK <+> dd)
    endK <- kw "end"
    pure (k <> arg <> mconcat whens <> def <+> endK)


----------------------------------------------------------------------
-- Function calls.
----------------------------------------------------------------------

-- | A function call as written: an ordinary call, or one with SQL's own
-- syntax (@extract(year from x)@, @x at time zone 'utc'@).
data Call
    = Plain Pg.FuncCall
    | Extract Pg.Node Pg.Node
    | Position Pg.Node Pg.Node
    | -- | @substring(x from a for b)@: the string and each keyword with its
      -- argument, in source order; or the comma form.
      Substring Pg.Node [(Text, Pg.Node)]
    | SubstringCommas [Pg.Node]
    | -- | @trim(both 'x' from y)@: modifier, characters, @from@, strings.
      Trim (Maybe Text) (Maybe Pg.Node) Bool [Pg.Node]
    | Overlay Pg.Node Pg.Node Pg.Node (Maybe Pg.Node)
    | AtTimeZone Pg.Node Pg.Node


classifyCall :: Pg.FuncCall -> R Call
classifyCall f = do
    names <- strsOf (f ^. Pg.funcname)
    at <- tokAt (fromIntegral (f ^. Pg.location))
    first <- tokL at
    let args = f ^. Pg.args
        sqlSyntax = f ^. Pg.funcformat == Pg.COERCE_SQL_SYNTAX
        keywordForm = case names of
            ["pg_catalog", _] -> first /= "pg_catalog" && first /= "\"pg_catalog\""
            -- postgresql-syntax reads @substring(a, b, c)@ as its own form.
            ["substring"] -> first == "substring"
            _ -> False
    if not (sqlSyntax || keywordForm)
        then pure (Plain f)
        else do
            when (hasExtras f) (unsupported "an SQL-syntax function with extras")
            open <- if names == ["pg_catalog", "timezone"] then pure (-1) else nextParen at
            close <- if open < 0 then pure (-1) else expectParen open
            pieces <- if open < 0 then pure [] else depthOneWords (open + 1) close
            case (names, args) of
                (["pg_catalog", "extract"], [field, x]) | sqlSyntax -> pure (Extract field x)
                (["pg_catalog", "position"], [haystack, needle]) | sqlSyntax -> pure (Position needle haystack)
                (["substring"], _) -> pure (SubstringCommas args)
                (["pg_catalog", "substring"], x : rest)
                    | not sqlSyntax -> pure (SubstringCommas args)
                    | otherwise -> do
                        -- Each argument after the string comes after a
                        -- @from@ or @for@; PostgreSQL adds a @from 1@ for a
                        -- lone @for@, which has no location.
                        let located = [r | r <- rest, not (null (locsNode r))]
                        keyed <- forM located \r -> do
                            s <- tokAt (minimum (locsNode r))
                            pure (lastBefore s pieces, r)
                        sorted <- sortByStart keyed
                        if all (\(k, _) -> k `elem` ["from", "for"]) sorted
                            then pure (Substring x sorted)
                            else unsupported "a substring pgfmt cannot read"
                ([_, fn], _) | fn `elem` ["btrim", "ltrim", "rtrim"], sqlSyntax -> do
                    let modifier = case [w | (_, w) <- pieces, w `elem` ["both", "leading", "trailing"]] of
                            w : _ -> Just w
                            [] -> Nothing
                        hasFrom = "from" `elem` map snd pieces
                    case args of
                        [s] -> pure (Trim modifier Nothing hasFrom [s])
                        [s, chars]
                            | hasFrom -> pure (Trim modifier (Just chars) True [s])
                            | otherwise -> pure (Trim modifier Nothing False [s, chars])
                        _ -> unsupported "a trim pgfmt cannot read"
                (["pg_catalog", "overlay"], [x, placing, from]) | sqlSyntax -> pure (Overlay x placing from Nothing)
                (["pg_catalog", "overlay"], [x, placing, from, for]) | sqlSyntax -> pure (Overlay x placing from (Just for))
                (["pg_catalog", "timezone"], [zone, x]) | sqlSyntax -> pure (AtTimeZone x zone)
                _ -> unsupported ("function " <> T.intercalate "." names <> " in SQL syntax")
  where
    lastBefore s pieces = case [w | (i, w) <- pieces, i < s] of
        [] -> ""
        ws -> last ws
    sortByStart xs = do
        keyed <- forM xs \(k, r) -> do
            s <- tokAt (minimum (locsNode r))
            pure (s, (k, r))
        pure (map snd (sortOnFst keyed))
    sortOnFst = foldr insert []
    insert x [] = [x]
    insert x (y : ys) = if fst x <= fst y then x : y : ys else y : insert x ys


-- | The keyword tokens directly inside a call's parentheses, with their
-- indices.
depthOneWords :: Int -> Int -> R [(Int, Text)]
depthOneWords from to = go from (0 :: Int)
  where
    go i depth
        | i >= to = pure []
        | otherwise = do
            t <- tokM i
            case t of
                Just tk
                    | tk.text `elem` ["(", "["] -> go (i + 1) (depth + 1)
                    | tk.text `elem` [")", "]"] -> go (i + 1) (depth - 1)
                    | depth == 0 && tk.isKeyword -> ((i, T.toLower tk.text) :) <$> go (i + 1) depth
                _ -> go (i + 1) depth


hasExtras :: Pg.FuncCall -> Bool
hasExtras f = isJust (f ^. Pg.maybe'aggFilter) || isJust (f ^. Pg.maybe'over) || f ^. Pg.aggWithinGroup


funcCall :: Mode -> E -> Pg.FuncCall -> R D
funcCall mode e f = do
    call <- classifyCall f
    case call of
        Plain _ -> plainCall mode e f
        AtTimeZone x zone -> do
            xd <- child e.en x >>= expr mode
            zd <- child e.en zone >>= expr mode
            k <- kw "at time zone"
            pure (xd <+> k <+> zd)
        _ -> do
            k <- kw (callName call)
            body <- callBody mode e call
            pure (k <> (if mode == MP then " (" else "(") <> body <> ")")


callName :: Call -> Text
callName = \case
    Plain _ -> ""
    Extract _ _ -> "extract"
    Position _ _ -> "position"
    Substring _ _ -> "substring"
    SubstringCommas _ -> "substring"
    Trim{} -> "trim"
    Overlay{} -> "overlay"
    AtTimeZone _ _ -> ""


-- | What goes between an SQL-syntax function's parentheses.
callBody :: Mode -> E -> Call -> R D
callBody mode e = \case
    Extract field x -> do
        i <- tokAt (minimum (locsNode field))
        t <- tokM i
        fd <- case t of
            Just tk
                | tk.kind == Pg.SCONST -> stringConst (fromMaybe "" (strConst field)) tk.text
                | T.toLower tk.text `elem` ["year", "month", "day", "hour", "minute", "second"] -> kw (T.toLower tk.text)
                | otherwise -> ident (fromMaybe "" (strConst field))
            Nothing -> unsupported "an extract field"
        xd <- child e.en x >>= expr mode
        k <- kw "from"
        pure (fd <+> k <+> xd)
    Position needle haystack -> do
        nd' <- child e.en needle >>= expr MP
        hd <- child e.en haystack >>= expr MP
        k <- kw "in"
        pure (nd' <+> k <+> hd)
    Substring x keyed -> do
        xd <- child e.en x >>= expr mode
        rest <- forM keyed \(k, r) -> do
            kd <- kw k
            rd <- child e.en r >>= expr mode
            pure (kd <+> rd)
        pure (spaceJoin (xd : rest))
    SubstringCommas args -> commaJoin <$> traverse (child e.en >=> expr mode) args
    Trim modifier chars hasFrom strings -> do
        md <- maybe (pure mempty) (fmap (<> " ") . kw) modifier
        cd <- maybe (pure Nothing) (fmap Just . (child e.en >=> expr mode)) chars
        sd <- commaJoin <$> traverse (child e.en >=> expr mode) strings
        fromK <- kw "from"
        pure $
            md <> case (cd, hasFrom) of
                (Just c, _) -> c <+> fromK <+> sd
                (Nothing, True) -> fromK <+> sd
                (Nothing, False) -> sd
    Overlay x placing from mFor -> do
        xd <- child e.en x >>= expr mode
        pd <- child e.en placing >>= expr mode
        fd <- child e.en from >>= expr mode
        forD <- maybe (pure mempty) (\n -> do k <- kw "for"; d <- child e.en n >>= expr mode; pure (" " <> k <+> d)) mFor
        placingK <- kw "placing"
        fromK <- kw "from"
        pure (xd <+> placingK <+> pd <+> fromK <+> fd <> forD)
    Plain _ -> unsupported "a plain call"
    AtTimeZone _ _ -> unsupported "at time zone"


strConst :: Pg.Node -> Maybe Text
strConst n = case node n of
    Just (Pg.Node'AConst c) | Just (Pg.A_Const'Sval s) <- c ^. Pg.maybe'val -> Just (s ^. Pg.sval)
    _ -> Nothing


-- | An ordinary call's name, and its opening parenthesis, placed.
callHead :: Pg.FuncCall -> R D
callHead f = do
    names <- strsOf (f ^. Pg.funcname)
    at <- tokAt (fromIntegral (f ^. Pg.location))
    open <- nextParen at
    _ <- expectParen open
    qualNameAt at names


-- | The arguments of an ordinary call: none, @*@, or a list.
data Params
    = NoParams
    | Star
    | Normal (Maybe Bool) [Pg.Node] [Pg.Node]
    | Variadic [Pg.Node] Pg.Node [Pg.Node]


callParams :: Pg.FuncCall -> R Params
callParams f
    | f ^. Pg.aggStar = pure Star
    | null args = pure NoParams
    | f ^. Pg.funcVariadic = pure (Variadic (init args) (last args) sort)
    | otherwise = do
        explicitAll <- do
            at <- tokAt (fromIntegral (f ^. Pg.location))
            open <- nextParen at
            tokIs (open + 1) "all"
        pure (Normal (if f ^. Pg.aggDistinct then Just True else if explicitAll then Just False else Nothing) args sort)
  where
    args = f ^. Pg.args
    sort = if f ^. Pg.aggWithinGroup then [] else f ^. Pg.aggOrder


plainCall :: Mode -> E -> Pg.FuncCall -> R D
plainCall mode e f = do
    app <- funcApplication mode e f
    within <-
        if f ^. Pg.aggWithinGroup
            then do
                k <- kw "within group"
                sd <- withinGroupParen f >> sortClause (f ^. Pg.aggOrder)
                pure (" " <> k <> " (" <> sd <> ")")
            else pure mempty
    filt <- case f ^. Pg.maybe'aggFilter of
        Nothing -> pure mempty
        Just x -> do
            filterParen f
            k <- kw "filter"
            w <- kw "where"
            xd <- child e.en x >>= expr mode
            pure (" " <> k <> " (" <> w <+> xd <> ")")
    over <- case f ^. Pg.maybe'over of
        Nothing -> pure mempty
        Just w -> (" " <>) <$> overClause f w
    pure (app <> within <> filt <> over)


-- | The parenthesis after a call's closing one and a keyword or two
-- (@within group (@, @filter (@).
afterCallParen :: Pg.FuncCall -> [Text] -> R ()
afterCallParen f words' = do
    at <- tokAt (fromIntegral (f ^. Pg.location))
    open <- nextParen at
    close <- matchOf open >>= req "a call"
    let find i = do
            t <- tokM i
            case t of
                Nothing -> unsupported "a call's clause pgfmt cannot find"
                Just _ -> do
                    ws <- traverse (\k -> tokL (i + k)) [0 .. length words' - 1]
                    if ws == words' then pure (i + length words') else find (i + 1)
    p <- find (close + 1)
    _ <- expectParen p
    pure ()


withinGroupParen :: Pg.FuncCall -> R ()
withinGroupParen f = afterCallParen f ["within", "group"]


filterParen :: Pg.FuncCall -> R ()
filterParen f = afterCallParen f ["filter"]


funcApplication :: Mode -> E -> Pg.FuncCall -> R D
funcApplication mode e f = do
    commented <- callCommented f
    if commented then funcApplicationWrapped e f else funcApplicationInline mode e f


-- | Whether a call has a comment between its parentheses.
callCommented :: Pg.FuncCall -> R Bool
callCommented f = do
    any' <- anyComments
    if not any'
        then pure False
        else do
            open <- tokAt (fromIntegral (f ^. Pg.location)) >>= nextParen
            close <- matchOf open
            maybe (pure False) (commentsBetween open) close


funcApplicationInline :: Mode -> E -> Pg.FuncCall -> R D
funcApplicationInline mode e f = do
    name <- callHead f
    params <- callParams f
    body <- case params of
        NoParams -> pure mempty
        Star -> txt "*"
        Normal distinct args sort -> do
            prefix <- distinctPrefix distinct
            ds <- traverse (funcArg mode e) args
            sd <- if null sort then pure mempty else (" " <>) <$> sortClause sort
            pure (prefix <> commaJoin ds <> sd)
        Variadic regular var sort -> do
            ds <- traverse (funcArg mode e) regular
            v <- funcArg mode e var
            k <- kw "variadic"
            sd <- if null sort then pure mempty else (" " <>) <$> sortClause sort
            pure ((if null ds then mempty else commaJoin ds <> ", ") <> k <> " " <> v <> sd)
    pure (name <> "(" <> body <> ")")


distinctPrefix :: Maybe Bool -> R D
distinctPrefix = \case
    Nothing -> pure mempty
    Just True -> (<> " ") <$> kw "distinct"
    Just False -> (<> " ") <$> kw "all"


-- | A call's argument, maybe named.
funcArg :: Mode -> E -> Pg.Node -> R D
funcArg mode e n = case node n of
    Just (Pg.Node'NamedArgExpr na) -> do
        (name, arrow) <- namedArg na
        arg <- req "a named argument" (na ^. Pg.maybe'arg) >>= child e.en >>= expr mode
        pure (name <> arrow <> arg)
    _ -> child e.en n >>= expr mode


namedArg :: Pg.NamedArgExpr -> R (D, D)
namedArg na = do
    i <- tokAt (fromIntegral (na ^. Pg.location))
    arrow <- tokL (i + 1)
    name <- ident (na ^. Pg.name)
    pure (name, if arrow == ":=" then " := " else " => ")


overClause :: Pg.FuncCall -> Pg.WindowDef -> R D
overClause f w = do
    k <- kw "over"
    if not (T.null (w ^. Pg.name))
        then (k <+>) <$> ident (w ^. Pg.name)
        else do
            afterCallParen f ["over"]
            spec <- windowSpec w
            pure (k <+> spec)


windowSpec :: Pg.WindowDef -> R D
windowSpec w = do
    ref <- if T.null (w ^. Pg.refname) then pure Nothing else Just <$> ident (w ^. Pg.refname)
    part <-
        if null (w ^. Pg.partitionClause)
            then pure Nothing
            else do
                k <- kw "partition by"
                ds <- traverse (top >=> expr MP) (w ^. Pg.partitionClause)
                pure (Just (k <+> commaJoin ds))
    order <- if null (w ^. Pg.orderClause) then pure Nothing else Just <$> sortClause (w ^. Pg.orderClause)
    frame <- frameClause w
    pure ("(" <> spaceJoin (catMaybes [ref, part, order, frame]) <> ")")


frameClause :: Pg.WindowDef -> R (Maybe D)
frameClause w
    | opts .&. 1 == 0 = pure Nothing
    | otherwise = do
        modeD <- kw if opts .&. 0x4 /= 0 then "rows" else if opts .&. 0x8 /= 0 then "groups" else "range"
        start' <- bound True
        extent <-
            if opts .&. 0x10 /= 0
                then do
                    end' <- bound False
                    b <- kw "between"
                    a <- kw "and"
                    pure (b <+> start' <+> a <+> end')
                else pure start'
        exclusion <-
            if
                | opts .&. 0x8000 /= 0 -> Just <$> kw "exclude current row"
                | opts .&. 0x10000 /= 0 -> Just <$> kw "exclude group"
                | opts .&. 0x20000 /= 0 -> Just <$> kw "exclude ties"
                | otherwise -> pure Nothing
        pure (Just (spaceJoin (catMaybes [Just modeD, Just extent, exclusion])))
  where
    opts = w ^. Pg.frameOptions
    bound isStart = do
        let (up, uf, cr, op, of') = if isStart then (0x20, 0x80, 0x200, 0x800, 0x2000) else (0x40, 0x100, 0x400, 0x1000, 0x4000)
            offset = if isStart then w ^. Pg.maybe'startOffset else w ^. Pg.maybe'endOffset
        if
            | opts .&. up /= 0 -> kw "unbounded preceding"
            | opts .&. uf /= 0 -> kw "unbounded following"
            | opts .&. cr /= 0 -> kw "current row"
            | opts .&. op /= 0 -> do
                o <- maybe (unsupported "a frame offset") (top >=> expr MP) offset
                (o <+>) <$> kw "preceding"
            | opts .&. of' /= 0 -> do
                o <- maybe (unsupported "a frame offset") (top >=> expr MP) offset
                (o <+>) <$> kw "following"
            | otherwise -> unsupported "a frame bound"


----------------------------------------------------------------------
-- Subqueries in expressions.
----------------------------------------------------------------------

-- | A subquery: the select, and how many parentheses the source has around
-- it (at least one).
data Sel = Sel
    { stmt :: Pg.SelectStmt
    , parens :: Int
    }


subLinkSel :: Pg.SubLink -> R Sel
subLinkSel s = do
    q <- req "a subquery" (s ^. Pg.maybe'subselect)
    st <- case node q of
        Just (Pg.Node'SelectStmt st) -> pure st
        _ -> unsupported "a subquery that is not a select"
    bound <- tokAt (fromIntegral (s ^. Pg.location))
    n <- selectParens bound st
    when (n < 1) (unsupported "a subquery without parentheses")
    pure Sel{stmt = st, parens = n}


subLink :: Mode -> E -> Pg.SubLink -> R D
subLink mode e s = case s ^. Pg.subLinkType of
    t | t `elem` [Pg.EXPR_SUBLINK, Pg.MULTIEXPR_SUBLINK] -> subquery
    Pg.EXISTS_SUBLINK -> do
        k <- kw "exists"
        q <- subquery
        pure (k <+> q)
    Pg.ARRAY_SUBLINK -> do
        k <- kw "array"
        q <- subquery
        pure (k <> (if mode == MP then " " else mempty) <> q)
    Pg.ANY_SUBLINK | null (s ^. Pg.operName) -> do
        arg <- req "an in" (s ^. Pg.maybe'testexpr) >>= child e.en
        inSubLink mode arg s False
    t | t `elem` [Pg.ANY_SUBLINK, Pg.ALL_SUBLINK] -> do
        names <- strsOf (s ^. Pg.operName)
        op <- case names of
            [o] -> pure o
            _ -> unsupported "a schema-qualified operator"
        at <- tokAt (fromIntegral (s ^. Pg.location))
        source <- tokL at
        unless (source == op || (op == "<>" && source == "!=")) (unsupported "an operator pgfmt cannot find")
        subType <- tokL (at + 1)
        unless (subType `elem` ["any", "some", "all"]) (unsupported "an any/all pgfmt cannot read")
        arg <- req "an any" (s ^. Pg.maybe'testexpr) >>= child e.en
        ad <- expr mode arg
        st <- kw subType
        q <- subquery
        pure (ad <+> pretty source <+> st <+> q)
    _ -> unsupported "a subquery pgfmt does not lay out"
  where
    subquery = do
        sel <- subLinkSel s
        if mode == MP then selP sel else selMaybeWrap sel


-- | @x in (select ...)@: "PgFmt.Render" hands its subquery to
-- postgresql-syntax's renderer whatever its length.
inSubLink :: Mode -> E -> Pg.SubLink -> Bool -> R D
inSubLink mode arg s negated = do
    ad <- expr mode arg
    k <- kw (if negated then "not in" else "in")
    sel <- subLinkSel s
    q <- selP sel
    pure (ad <+> k <+> q)


----------------------------------------------------------------------
-- Expressions, wrapped: "PgFmt.Render"'s rules, over libpg_query's nodes.
----------------------------------------------------------------------

-- | One parenthesis level off an expression, as postgresql-syntax's
-- @InParensCExpr@ would hold it, with the indirection after it if any.
inParens :: E -> R (Maybe (E, Maybe Pg.A_Indirection))
inParens e
    | e.lv > 0 = pure (Just (e{lv = e.lv - 1}, Nothing))
    | Just (Pg.Node'AIndirection ind) <- node e.en
    , Just argN <- ind ^. Pg.maybe'arg = do
        arg <- child e.en argN
        pure
            if arg.lv > 0
                then Just (arg{lv = arg.lv - 1}, Just ind)
                else Nothing
    | otherwise = pure Nothing


-- | A @::@ cast, not in parentheses: its argument and type.
asColonCast :: E -> R (Maybe (E, Pg.TypeName))
asColonCast e
    | e.lv == 0
    , Just (Pg.Node'TypeCast tc) <- node e.en
    , Just tn <- tc ^. Pg.maybe'typeName
    , Just arg <- tc ^. Pg.maybe'arg = do
        form <- castForm tc
        case form of
            ColonCast -> Just . (,tn) <$> child e.en arg
            _ -> pure Nothing
    | otherwise = pure Nothing


-- | An ordinary call with no @within group@, @filter@ or @over@.
asFuncApp :: E -> R (Maybe Pg.FuncCall)
asFuncApp e
    | e.lv == 0
    , Just (Pg.Node'FuncCall f) <- node e.en
    , not (hasExtras f) = do
        call <- classifyCall f
        pure case call of
            Plain _ -> Just f
            _ -> Nothing
    | otherwise = pure Nothing


-- | postgresql-syntax's @func_expr_common_subexpr@ forms.
data Subexpr
    = SubCoalesce [Pg.Node]
    | SubGreatest [Pg.Node]
    | SubLeast [Pg.Node]
    | SubNullIf Pg.Node Pg.Node
    | SubCast Pg.Node Pg.TypeName
    | -- | @extract@, @position@, @substring@, @trim@, @overlay@.
      SubSyntax
    | -- | @current_date@ and the like.
      SubValue


asSubexpr :: E -> R (Maybe Subexpr)
asSubexpr e
    | e.lv > 0 = pure Nothing
    | otherwise = case node e.en of
        Just (Pg.Node'CoalesceExpr c) -> pure (Just (SubCoalesce (c ^. Pg.args)))
        Just (Pg.Node'MinMaxExpr c) -> pure (Just ((if c ^. Pg.op == Pg.IS_GREATEST then SubGreatest else SubLeast) (c ^. Pg.args)))
        Just (Pg.Node'AExpr a)
            | a ^. Pg.kind == Pg.AEXPR_NULLIF
            , Just l <- a ^. Pg.maybe'lexpr
            , Just r <- a ^. Pg.maybe'rexpr ->
                pure (Just (SubNullIf l r))
        Just (Pg.Node'TypeCast tc)
            | Just tn <- tc ^. Pg.maybe'typeName
            , Just arg <- tc ^. Pg.maybe'arg -> do
                form <- castForm tc
                pure case form of
                    CastCall -> Just (SubCast arg tn)
                    _ -> Nothing
        Just (Pg.Node'FuncCall f) | not (hasExtras f) -> do
            call <- classifyCall f
            pure case call of
                Plain _ -> Nothing
                AtTimeZone _ _ -> Nothing
                _ -> Just SubSyntax
        Just (Pg.Node'SqlvalueFunction _) -> pure (Just SubValue)
        _ -> pure Nothing


isWrapEligible :: E -> R Bool
isWrapEligible e = do
    paren <- inParens e
    cast <- asColonCast e
    app <- asFuncApp e
    subexpr <- asSubexpr e
    case (paren, cast, app, subexpr) of
        (Just (inner, _), _, _, _) -> isWrapEligible inner
        (_, Just (inner, _), _, _) -> isWrapEligible inner
        (_, _, Just _, _) -> pure True
        (_, _, _, Just SubValue) -> pure False
        (_, _, _, Just _) -> pure True
        _ -> case node e.en of
            _ | e.lv > 0 -> pure False
            Just (Pg.Node'AArrayExpr a) -> do
                keyword <- arrayKeyword a
                if keyword then arrayEligible e a else pure False
            Just (Pg.Node'RowExpr r) -> anyM (child e.en >=> isWrapEligible) (r ^. Pg.args)
            Just (Pg.Node'CaseExpr _) -> pure True
            _ -> pure False


arrayEligible :: E -> Pg.A_ArrayExpr -> R Bool
arrayEligible e a = anyM el (a ^. Pg.elements)
  where
    el n = case node n of
        Just (Pg.Node'AArrayExpr sub) -> do
            keyword <- arrayKeyword sub
            if keyword then child e.en n >>= isWrapEligible else child e.en n >>= \se -> arrayEligible se sub
        _ -> child e.en n >>= isWrapEligible


anyM :: (a -> R Bool) -> [a] -> R Bool
anyM _ [] = pure False
anyM f (x : xs) = f x >>= \b -> if b then pure True else anyM f xs


-- | A call (maybe under @::@ casts) and how to put the casts back.
topLevelFuncApp :: E -> R (Maybe (Pg.FuncCall, E, D -> R D))
topLevelFuncApp e = do
    cast <- asColonCast e
    case cast of
        Just (inner, tn) -> do
            found <- topLevelFuncApp inner
            pure case found of
                Just (f, fe, attach) -> Just (f, fe, \d -> do d' <- attach d; t <- typeName tn; pure (d' <> "::" <> t))
                Nothing -> Nothing
        Nothing -> do
            app <- asFuncApp e
            pure ((\f -> (f, e, pure)) <$> app)


funcAppArgCount :: Pg.FuncCall -> Int
funcAppArgCount f
    | f ^. Pg.aggStar = 0
    | otherwise = length (f ^. Pg.args)


-- | Arguments shaped like keys and values: at least four, an even number,
-- every other one from the first a string literal.
looksLikeKeyValuePairs :: E -> [Pg.Node] -> R Bool
looksLikeKeyValuePairs e args
    | n < 4 || odd n = pure False
    | otherwise = allM key [a | (i, a) <- zip [0 :: Int ..] args, even i]
  where
    n = length args
    key a = case node a of
        Just (Pg.Node'AConst c) | Just (Pg.A_Const'Sval _) <- c ^. Pg.maybe'val -> do
            ae <- child e.en a
            source <- tokAt (fromIntegral (c ^. Pg.location)) >>= tokM
            pure (ae.lv == 0 && maybe False (\t -> "'" `T.isPrefixOf` t.text || "$" `T.isPrefixOf` t.text) source)
        _ -> pure False


allM :: (a -> R Bool) -> [a] -> R Bool
allM f = fmap not . anyM (fmap not . f)


-- | Whether an expression is a call (maybe under @::@ casts) with key/value
-- shaped arguments.
hasPairShapedCall :: E -> R Bool
hasPairShapedCall e = do
    found <- topLevelFuncApp e
    case found of
        Just (f, fe, _) -> do
            params <- callParams f
            case params of
                Normal _ args _ -> looksLikeKeyValuePairs fe args
                _ -> pure False
        Nothing -> pure False


inlineLengthThreshold, selectTargetInlineThreshold, subqueryInlineThreshold, funcWrapThreshold :: Int
inlineLengthThreshold = 60
selectTargetInlineThreshold = 150
subqueryInlineThreshold = 100
funcWrapThreshold = 2


exprMaybeWrap :: E -> R D
exprMaybeWrap e = do
    inline <- expr MR e
    force <- hasPairShapedCall e
    commented <- hasCmts e.en
    eligible <- isWrapEligible e
    if (len inline > inlineLengthThreshold || force || commented) && eligible
        then exprWrapped e
        else pure inline


exprWrapped :: E -> R D
exprWrapped e = do
    paren <- inParens e
    cast <- asColonCast e
    app <- asFuncApp e
    subexpr <- asSubexpr e
    case (paren, cast, app, subexpr) of
        (Just (inner, ind), _, _, _) -> inParensWrapped e inner ind
        (_, Just (inner, tn), _, _) -> do
            d <- exprWrapped inner
            t <- typeName tn
            pure (d <> "::" <> t)
        (_, _, Just f, _) -> funcApplicationWrapped e f
        (_, _, _, Just sub) -> subexprWrapped e sub
        _ -> case node e.en of
            Just (Pg.Node'AArrayExpr a) -> do
                keyword <- arrayKeyword a
                if keyword
                    then do
                        k <- kw "array"
                        body <- arrayWrapped e a
                        pure (k <> body)
                    else expr MR e
            Just (Pg.Node'RowExpr r) -> do
                at <- tokAt (fromIntegral (r ^. Pg.location))
                explicit <- tokIs at "row"
                open <- if explicit then nextParen at else pure at
                _ <- expectParen open
                args <- traverse (child e.en >=> exprMaybeWrap) (r ^. Pg.args)
                k <- kw "row"
                pure ((if explicit then k <> "(" else "(") <> nest indentSize (argRows args) <> hardline <> ")")
            Just (Pg.Node'CaseExpr c) -> caseWrapped e c
            _ -> expr MR e


inParensWrapped :: E -> E -> Maybe Pg.A_Indirection -> R D
inParensWrapped outer inner mInd = do
    isCase <- case node inner.en of
        Just (Pg.Node'CaseExpr c) | inner.lv == 0 -> pure (Just c)
        _ -> pure Nothing
    case (mInd, isCase) of
        (Nothing, Just c) -> do
            body <- caseWrapped inner c
            pure (align ("(" <> body <> ")"))
        _ -> do
            topCase <- isTopLevelCase inner
            body <- if topCase then exprWrapped inner else exprMaybeWrap inner
            ind <- maybe (pure mempty) (\i -> indirection MR outer.en (i ^. Pg.indirection)) mInd
            pure ("(" <> nest indentSize (hardline <> body) <> hardline <> ")" <> ind)


isTopLevelCase :: E -> R Bool
isTopLevelCase e = do
    paren <- inParens e
    cast <- asColonCast e
    case (paren, cast) of
        (Just (inner, _), _) -> isTopLevelCase inner
        (_, Just (inner, _)) -> isTopLevelCase inner
        _ -> pure case node e.en of
            Just (Pg.Node'CaseExpr _) -> e.lv == 0
            _ -> False


argRows :: [D] -> D
argRows = \case
    [] -> mempty
    (x : xs) -> (hardline <> "  " <> x) <> mconcat [hardline <> ", " <> r | r <- xs]


-- | 'argRows', and the line break before the closing parenthesis (to go
-- outside the rows' nesting), with the comments before each argument and
-- before the parenthesis, if each argument's first token and the
-- parenthesis are known.
argRowsAt :: Maybe ([Int], Int) -> [D] -> R (D, D)
argRowsAt Nothing ds = pure (argRows ds, hardline)
argRowsAt (Just (firsts, close)) ds = do
    rows <- forM (zip3 [0 :: Int ..] firsts ds) \(i, q, d) -> do
        b <- brk "  " q
        pure (b <> (if i == 0 then "  " else ", ") <> d)
    end <- closeBrk close
    pure (mconcat rows, end)


-- | The line break before a list's closing bracket, outside the list's
-- nesting: comments before it line up with the list's items.
closeBrk :: Int -> R D
closeBrk = brk (pretty (T.replicate (indentSize + 2) " "))


-- | 'argPairRows' and the line break before the closing parenthesis, like
-- 'argRowsAt': comments go before a key, or after its value.
argPairRowsAt :: Maybe ([Int], Int) -> [D] -> R (D, D)
argPairRowsAt Nothing ds = pure (argPairRows ds, hardline)
argPairRowsAt (Just (firsts, close)) ds = do
    -- A comment between a key and its value has nowhere to go.
    inside <- or <$> forM (pairs firsts) \(_, v) -> not . null <$> gapBefore v
    if inside
        then pure (argPairRows ds, hardline)
        else do
            rows <- forM (zip3 [0 :: Int ..] (map fst (pairs firsts)) (pairs ds)) \(i, q, (k, v)) -> do
                b <- brk "  " q
                pure (b <> (if i == 0 then "  " else ", ") <> k <> ", " <> v)
            end <- closeBrk close
            pure (mconcat rows, end)
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []


argPairRows :: [D] -> D
argPairRows = goFirst . pairs
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []
    goFirst [] = mempty
    goFirst ((k, v) : rest) =
        (hardline <> "  " <> k <> ", " <> v)
            <> mconcat [hardline <> ", " <> k' <> ", " <> v' | (k', v') <- rest]


-- | The first token of each of a call's arguments, and its closing
-- parenthesis.
callArgsAt :: Pg.FuncCall -> Int -> R (Maybe ([Int], Int))
callArgsAt f n = do
    any' <- anyComments
    if not any'
        then pure Nothing
        else do
            at <- tokAt (fromIntegral (f ^. Pg.location))
            open <- nextParen at
            mclose <- matchOf open
            case mclose of
                Nothing -> pure Nothing
                Just close -> do
                    lead <- tokL (open + 1)
                    let open' = if lead `elem` ["distinct", "all"] then open + 1 else open
                    firsts <- listFirsts open' close n
                    pure ((,close) <$> firsts)


funcApplicationWrapped :: E -> Pg.FuncCall -> R D
funcApplicationWrapped e f = do
    name <- callHead f
    params <- callParams f
    (body, end) <- case params of
        NoParams -> pure (mempty, hardline)
        Star -> pure (hardline <> "*", hardline)
        Normal distinct args sort -> do
            prefix <- distinctPrefix distinct
            ds <- traverse (funcArgMaybeWrap e) args
            pairShaped <- looksLikeKeyValuePairs e args
            if null sort
                then do
                    at <- callArgsAt f (length args)
                    (rows, end) <- (if pairShaped then argPairRowsAt else argRowsAt) at ds
                    pure (prefix <> rows, end)
                else do
                    sd <- (hardline <>) <$> sortClause sort
                    pure (prefix <> (if pairShaped then argPairRows ds else argRows ds) <> sd, hardline)
        Variadic regular var sort -> do
            ds <- traverse (funcArgMaybeWrap e) regular
            v <- funcArgMaybeWrap e var
            k <- kw "variadic"
            sd <- if null sort then pure mempty else (hardline <>) <$> sortClause sort
            pure (argRows (ds ++ [k <> " " <> v]) <> sd, hardline)
    pure (name <> "(" <> nest indentSize body <> end <> ")")


funcArgMaybeWrap :: E -> Pg.Node -> R D
funcArgMaybeWrap e n = do
    inline <- funcArg MR e n
    (prefix, argN) <- case node n of
        Just (Pg.Node'NamedArgExpr na) -> do
            (name, arrow) <- namedArg na
            a <- req "a named argument" (na ^. Pg.maybe'arg)
            pure (name <> arrow, a)
        _ -> pure (mempty, n)
    arg <- child e.en argN
    force <- hasPairShapedCall arg
    commented <- hasCmts argN
    eligible <- isWrapEligible arg
    if (len inline > inlineLengthThreshold || force || commented) && eligible
        then (prefix <>) <$> exprWrapped arg
        else pure inline


subexprWrapped :: E -> Subexpr -> R D
subexprWrapped e = \case
    SubCoalesce args -> rows "coalesce" loc args
    SubGreatest args -> rows "greatest" loc args
    SubLeast args -> rows "least" loc args
    SubNullIf l r -> rows "nullif" loc [l, r]
    SubCast arg tn -> do
        open <- castLoc >>= nextParen
        _ <- expectParen open
        a <- child e.en arg >>= exprMaybeWrap
        t <- typeName tn
        k <- kw "cast"
        as <- kw "as"
        pure (k <> "(" <> nest indentSize (hardline <> a <+> as <+> t) <> hardline <> ")")
    _ -> expr MR e
  where
    loc = case node e.en of
        Just (Pg.Node'CoalesceExpr x) -> x ^. Pg.location
        Just (Pg.Node'MinMaxExpr x) -> x ^. Pg.location
        Just (Pg.Node'AExpr x) -> x ^. Pg.location
        _ -> -1
    castLoc = case node e.en of
        Just (Pg.Node'TypeCast tc) -> tokAt (fromIntegral (tc ^. Pg.location))
        _ -> unsupported "a cast"
    rows name at args = do
        open <- tokAt (fromIntegral at) >>= nextParen
        _ <- expectParen open
        ds <- traverse (child e.en >=> exprMaybeWrap) args
        k <- kw name
        pure (k <> "(" <> nest indentSize (argRows ds) <> hardline <> ")")


arrayWrapped :: E -> Pg.A_ArrayExpr -> R D
arrayWrapped e a = do
    let els = a ^. Pg.elements
    nested <- case els of
        n : _ | Just (Pg.Node'AArrayExpr sub) <- node n -> not <$> arrayKeyword sub
        _ -> pure False
    if null els
        then pure "[]"
        else do
            ds <-
                if nested
                    then forM els \n -> case node n of
                        Just (Pg.Node'AArrayExpr sub) -> do
                            se <- child e.en n
                            k <- kw "array"
                            (k <>) <$> arrayWrapped se sub
                        _ -> unsupported "a nested array pgfmt cannot read"
                    else traverse (child e.en >=> exprMaybeWrap) els
            keyword <- arrayKeyword a
            at <- tokAt (fromIntegral (a ^. Pg.location))
            positions <- commentedList (if keyword then at + 1 else at) (length els)
            (rows, end) <- argRowsAt positions ds
            pure ("[" <> nest indentSize rows <> end <> "]")


caseWrapped :: E -> Pg.CaseExpr -> R D
caseWrapped e c = do
    k <- kw "case"
    arg <- maybe (pure mempty) (\a -> (" " <>) <$> (child e.en a >>= expr MR)) (c ^. Pg.maybe'arg)
    whens <- forM (c ^. Pg.args) \w -> case node w of
        Just (Pg.Node'CaseWhen cw) -> do
            cond <- req "a when" (cw ^. Pg.maybe'expr) >>= child e.en >>= exprMaybeWrap
            result <- req "a when" (cw ^. Pg.maybe'result) >>= child e.en >>= exprForceSubqueryWrap
            whenK <- kw "when"
            thenK <- kw "then"
            pure (hardline <> whenK <+> cond <+> thenK <+> result)
        _ -> unsupported "a case branch pgfmt cannot read"
    def <- case c ^. Pg.maybe'defresult of
        Nothing -> pure mempty
        Just d -> do
            elseK <- kw "else"
            dd <- child e.en d >>= exprForceSubqueryWrap
            pure (hardline <> elseK <+> dd)
    endK <- kw "end"
    pure (k <> arg <> nest indentSize (mconcat whens <> def) <> hardline <> endK)


exprForceSubqueryWrap :: E -> R D
exprForceSubqueryWrap e
    | e.lv == 0, Just (Pg.Node'SubLink s) <- node e.en, s ^. Pg.subLinkType == Pg.EXPR_SUBLINK =
        subLinkSel s >>= selWrapped
    | e.lv == 0
    , Just (Pg.Node'AIndirection ind) <- node e.en
    , Just argN <- ind ^. Pg.maybe'arg
    , Just (Pg.Node'SubLink s) <- node argN
    , s ^. Pg.subLinkType == Pg.EXPR_SUBLINK = do
        arg <- child e.en argN
        if arg.lv > 0
            then exprMaybeWrap e
            else do
                q <- subLinkSel s >>= selWrapped
                i <- indirection MR e.en (ind ^. Pg.indirection)
                pure (q <> i)
    | otherwise = do
        cast <- asColonCast e
        case cast of
            Just (inner, tn) -> do
                d <- exprForceSubqueryWrap inner
                t <- typeName tn
                pure (d <> "::" <> t)
            Nothing -> exprMaybeWrap e


-- | A select target or SET value: "PgFmt.Render"'s @renderExprDoc@.
exprDoc :: E -> R D
exprDoc e = do
    found <- topLevelFuncApp e
    case found of
        Just (f, fe, attach)
            | funcAppArgCount f >= funcWrapThreshold -> funcApplicationWrapped fe f >>= attach
        _ -> do
            topCase <- isTopLevelCase e
            if topCase
                then align <$> exprWrapped e
                else do
                    inline <- expr MR e
                    eligible <- isWrapEligible e
                    commented <- hasCmts e.en
                    if (len inline > selectTargetInlineThreshold || commented) && eligible
                        then exprWrapped e
                        else pure inline


----------------------------------------------------------------------
-- Conditions.
----------------------------------------------------------------------

-- | @where@ / @having@ / @on@: the first condition after the keyword, each
-- further top-level @and@ on its own line, right-aligned to the keyword.
condAligned :: Int -> Text -> Pg.Node -> R D
condAligned anchor keyword n = do
    commented <- hasCmts n
    if commented then condCommented anchor keyword n else condPlain anchor keyword n


-- | 'condAligned' for a condition with comments in it: split on its
-- top-level @and@s by the tree rather than the text, the comments before
-- each @and@ at the line break before it.
condCommented :: Int -> Text -> Pg.Node -> R D
condCommented anchor keyword n = do
    e <- top n
    let kwPad = pretty (T.replicate (max 0 (anchor - T.length keyword)) " ")
        andPad = pretty (T.replicate (max 0 (anchor - 3)) " ")
    k <- kw keyword
    case node n of
        Just (Pg.Node'BoolExpr b) | e.lv == 0, b ^. Pg.boolop == Pg.AND_EXPR, first : rest <- b ^. Pg.args -> do
            andK <- kw "and"
            fd <- child n first >>= exprMaybeWrap
            rds <- forM rest \x -> do
                xd <- child n x >>= exprMaybeWrap
                andAt <- clauseAt [["and"]] x
                -- The comments before the @and@ (one after it has no
                -- place).
                b' <- (<> (andPad <> andK)) <$> brkAt andPad andAt
                pure (b' <+> xd)
            pure (kwPad <> k <+> fd <> mconcat rds)
        _ -> do
            d <- exprMaybeWrap e
            pure (kwPad <> k <+> d)


condPlain :: Int -> Text -> Pg.Node -> R D
condPlain anchor keyword n = do
    e <- top n
    lower <- local (\env -> env{upper = False}) (expr MR e)
    cased <- expr MR e
    let parts = splitOnAnd (renderToText lower) (renderToText cased)
        kwPad = pretty (T.replicate (max 0 (anchor - T.length keyword)) " ")
        andPad = pretty (T.replicate (max 0 (anchor - 3)) " ")
    k <- kw keyword
    andK <- kw "and"
    eligible <- isWrapEligible e
    commented <- hasCmts n
    case parts of
        [single]
            | (T.length single > inlineLengthThreshold || commented) && eligible -> do
                d <- exprMaybeWrap e
                pure (kwPad <> k <+> d)
            | otherwise -> pure (kwPad <> k <+> raw single)
        (first : rest) -> pure (vsep ((kwPad <> k <+> raw first) : map (\p -> andPad <> andK <+> raw p) rest))
        [] -> pure mempty


-- | "PgFmt.Render"'s @splitOnAnd@, deciding on the lower-case rendering and
-- cutting the one in the requested case (the same text but for the case of
-- its keywords).
splitOnAnd :: Text -> Text -> [Text]
splitOnAnd lower cased = go 0 False Nothing "" (zip (T.unpack lower) (T.unpack cased))
  where
    go :: Int -> Bool -> Maybe Char -> String -> [(Char, Char)] -> [Text]
    go _ _ _ acc [] = [T.strip (T.pack (reverse acc))]
    go d btwn (Just q) acc ((c1, o1) : (c2, o2) : rest)
        | c1 == q && c2 == q = go d btwn (Just q) (o2 : o1 : acc) rest
    go d btwn (Just q) acc ((c, o) : rest)
        | c == q = go d btwn Nothing (o : acc) rest
        | otherwise = go d btwn (Just q) (o : acc) rest
    go d btwn Nothing acc (('\'', o) : rest) = go d btwn (Just '\'') (o : acc) rest
    go d btwn Nothing acc (('"', o) : rest) = go d btwn (Just '"') (o : acc) rest
    go d btwn Nothing acc (('(', o) : rest) = go (d + 1) btwn Nothing (o : acc) rest
    go d btwn Nothing acc ((')', o) : rest) = go (max 0 (d - 1)) btwn Nothing (o : acc) rest
    go 0 True Nothing acc ((' ', a) : ('a', b) : ('n', c) : ('d', d') : (' ', e) : rest) =
        go 0 False Nothing (e : d' : c : b : a : acc) rest
    go 0 _ Nothing acc ((' ', _) : ('a', _) : ('n', _) : ('d', _) : (' ', _) : rest) =
        T.strip (T.pack (reverse acc)) : go 0 False Nothing "" rest
    go d _ Nothing acc (('b', a) : ('e', b) : ('t', c) : ('w', d') : ('e', e) : ('e', f) : ('n', g) : (' ', h) : rest) =
        go d True Nothing (h : g : f : e : d' : c : b : a : acc) rest
    go d btwn st acc ((_, o) : rest) = go d btwn st (o : acc) rest


----------------------------------------------------------------------
-- Selects.
----------------------------------------------------------------------

-- | A select's first token: @with@, @select@, @values@, or the opening
-- parenthesis of its first operand.
selectFirst :: Pg.SelectStmt -> R Int
selectFirst s
    | Just w <- s ^. Pg.maybe'withClause = do
        i <- tokAt (fromIntegral (w ^. Pg.location))
        ok <- tokIs i "with"
        unless ok (unsupported "a with clause pgfmt cannot find")
        pure i
    | s ^. Pg.op /= Pg.SETOP_NONE = do
        l <- req "a set operation" (s ^. Pg.maybe'larg)
        r <- req "a set operation" (s ^. Pg.maybe'rarg)
        f <- selectFirst l
        rls <- nonEmptyLocs (locsMsg r)
        rStart <- tokAt (minimum rls)
        extendOver f rStart
    | otherwise = do
        ls <- nonEmptyLocs (locsMsg s)
        i <- tokAt (minimum ls)
        let isValues = not (null (s ^. Pg.valuesLists))
            target = if isValues then "values" else "select"
            skippable = if isValues then ["("] else ["(", "distinct", "all", "on"] ++ ["from" | null (s ^. Pg.targetList)]
            walk j = do
                w <- tokL j
                if
                    | w == target -> pure j
                    | w `elem` skippable -> walk (j - 1)
                    | otherwise -> unsupported "a select pgfmt cannot find the start of"
        walk (i - 1)


nonEmptyLocs :: [Int] -> R [Int]
nonEmptyLocs ls = if null ls then unsupported "a node without a location" else pure ls


-- | Back over the opening parentheses before a token that close before
-- another.
extendOver :: Int -> Int -> R Int
extendOver f bound = do
    isParen <- tokIs (f - 1) "("
    m <- matchOf (f - 1)
    case m of
        Just close | isParen, close < bound -> extendOver (f - 1) bound
        _ -> pure f


-- | How many parentheses the source has around a select, at or after a
-- token, placing them.
selectParens :: Int -> Pg.SelectStmt -> R Int
selectParens bound s = do
    ls <- nonEmptyLocs (locsMsg s)
    remembered (2, minimum ls, maximum ls, length ls) (selectParens' bound s ls)


-- | Work something out about a node once: what places parentheses must give
-- the same answer when asked again.
remembered :: (Int, Int, Int, Int) -> R Int -> R Int
remembered key compute = do
    known <- gets (Map.lookup key . (.memo))
    case known of
        Just k -> pure k
        Nothing -> do
            k <- compute
            modify' \st -> st{memo = Map.insert key k st.memo}
            pure k


selectParens' :: Int -> Pg.SelectStmt -> [Int] -> R Int
selectParens' bound s ls = do
    f <- selectFirst s
    let set = IS.fromList ls
    maxTok <- tokAt (maximum ls)
    let go p n
            | p < bound = pure n
            | otherwise = do
                isParen <- tokIs p "("
                placed <- isConsumed p
                mm <- matchOf p
                case mm of
                    Just m | isParen, not placed, m > maxTok -> do
                        cov <- covers p m set
                        if cov then consume p >> go (p - 1) (n + 1) else pure n
                    _ -> pure n
    go (f - 1) 0


-- | The parentheses around a select, not placing them: for finding where
-- a table reference starts.
selectParensPeek :: Pg.SelectStmt -> R Int
selectParensPeek s = do
    f <- selectFirst s
    ls <- nonEmptyLocs (locsMsg s)
    let set = IS.fromList ls
    maxTok <- tokAt (maximum ls)
    let go p n = do
            isParen <- tokIs p "("
            mm <- matchOf p
            case mm of
                Just m | isParen, m > maxTok -> do
                    cov <- covers p m set
                    if cov then go (p - 1) (n + 1) else pure n
                _ -> pure n
    go (f - 1) 0


-- | "PgFmt.Render"'s @renderSelectStmt@: a statement-level select, with
-- however many parentheses around it.
selStmt :: Pg.SelectStmt -> Int -> R D
selStmt s 0 = selNoParens s
selStmt s n = selWithParens s n


selWithParens :: Pg.SelectStmt -> Int -> R D
selWithParens s n = do
    inner <- if n <= 1 then selNoParens s else selWithParens s (n - 1)
    pure ("(" <> inner <> ")")


selNoParens :: Pg.SelectStmt -> R D
selNoParens s = do
    w <- traverse withClause (s ^. Pg.maybe'withClause)
    c <- selClause s
    so <- if null (s ^. Pg.sortClause) then pure Nothing else Just <$> sortClause (s ^. Pg.sortClause)
    l <- selectLimit True s
    lk <- locking s
    cAt <- attempt (selectFirst (s & Pg.maybe'withClause .~ Nothing))
    soAt <- maybe (pure Nothing) (clauseAt [["order", "by"]]) (listToMaybe (s ^. Pg.sortClause))
    lAt <- limitAt s
    linesAt mempty (present [(Nothing, w), (cAt, Just c), (soAt, so), (lAt, l), (Nothing, lk)])


-- | The clauses there are, with their first tokens.
present :: [(Maybe Int, Maybe D)] -> [(Maybe Int, D)]
present xs = [(q, d) | (q, Just d) <- xs]


-- | Where a select's @limit@ or @offset@, whichever comes first, starts.
limitAt :: Pg.SelectStmt -> R (Maybe Int)
limitAt s = do
    l <- maybe (pure Nothing) (clauseAt [["limit"]]) (s ^. Pg.maybe'limitCount)
    o <- maybe (pure Nothing) (clauseAt [["offset"]]) (s ^. Pg.maybe'limitOffset)
    pure case catMaybes [l, o] of
        [] -> Nothing
        qs -> Just (minimum qs)


-- | A set operation's operands, with their parentheses.
operands :: Pg.SelectStmt -> R (Pg.SelectStmt, Int, Pg.SelectStmt, Int)
operands s = do
    l <- req "a set operation" (s ^. Pg.maybe'larg)
    r <- req "a set operation" (s ^. Pg.maybe'rarg)
    lp <- selectParens 0 l
    rp <- selectParens 0 r
    let bare x n = n == 0 && (isJust (x ^. Pg.maybe'withClause) || not (null (x ^. Pg.sortClause)) || isJust (x ^. Pg.maybe'limitCount) || isJust (x ^. Pg.maybe'limitOffset) || not (null (x ^. Pg.lockingClause)))
    when (bare l lp || bare r rp) (unsupported "a set operation operand pgfmt cannot read")
    pure (l, lp, r, rp)


-- | @union all@ and the like, as written, and where.
setOperator :: Pg.SelectStmt -> Pg.SelectStmt -> Int -> R (D, Int)
setOperator s r rp = do
    rFirst <- selectFirst r
    let k = rFirst - rp - 1
    w <- tokL k
    (opTok, modifier) <-
        if w `elem` ["all", "distinct"] then pure (k - 1, Just w) else pure (k, Nothing)
    op <- tokL opTok
    let expected = case s ^. Pg.op of
            Pg.SETOP_UNION -> "union"
            Pg.SETOP_INTERSECT -> "intersect"
            _ -> "except"
    unless (op == expected && (s ^. Pg.all) == (modifier == Just "all")) (unsupported "a set operator pgfmt cannot find")
    d <- kw (T.unwords (op : toList modifier))
    pure (d, opTok)


-- | "PgFmt.Render"'s @renderSelectClause@ / @renderSimpleSelect@.
selClause :: Pg.SelectStmt -> R D
selClause s
    | s ^. Pg.op /= Pg.SETOP_NONE = do
        (l, lp, r, rp) <- operands s
        ld <- if lp == 0 then selClause l else selWithParens l lp
        (op, opAt) <- setOperator s r rp
        rd <- if rp == 0 then selClause r else selWithParens r rp
        rAt <- attempt (subtract rp <$> selectFirst r)
        linesAt mempty [(Nothing, ld), (Just opAt, op), (rAt, rd)]
    | not (null (s ^. Pg.valuesLists)) = valuesClause s
    | otherwise = do
        when (isJust (s ^. Pg.maybe'intoClause)) (unsupported "select into")
        t <- targeting s
        f <- if null (s ^. Pg.fromClause) then pure Nothing else Just <$> fromClause (s ^. Pg.fromClause)
        w <- traverse (condAligned 5 "where") (s ^. Pg.maybe'whereClause)
        g <- groupClause s
        h <- traverse (condAligned 6 "having") (s ^. Pg.maybe'havingClause)
        wi <- windowClause s
        fAt <- maybe (pure Nothing) (clauseAt [["from"], ["from", "lateral"]]) (listToMaybe (s ^. Pg.fromClause))
        wAt <- maybe (pure Nothing) (clauseAt [["where"]]) (s ^. Pg.maybe'whereClause)
        gAt <- maybe (pure Nothing) (clauseAt [["group", "by"]]) (listToMaybe (s ^. Pg.groupClause))
        hAt <- maybe (pure Nothing) (clauseAt [["having"]]) (s ^. Pg.maybe'havingClause)
        wiAt <- maybe (pure Nothing) (clauseAt [["window"]]) (listToMaybe (s ^. Pg.windowClause))
        linesAt mempty (present [(Nothing, Just t), (fAt, f), (wAt, w), (gAt, g), (hAt, h), (wiAt, wi)])


-- | @select@, @select all@, @select distinct@, @select distinct on (...)@.
targetPrefix :: Mode -> Pg.SelectStmt -> R D
targetPrefix mode s = do
    sel <- kw "select"
    case s ^. Pg.distinctClause of
        [] -> do
            explicitAll <- case s ^. Pg.targetList of
                t : _ | Just (Pg.Node'ResTarget rt) <- node t -> tokAt (fromIntegral (rt ^. Pg.location)) >>= \i -> tokIs (i - 1) "all"
                _ -> pure False
            if explicitAll then (sel <+>) <$> kw "all" else pure sel
        [x] | isNothing (node x) -> (sel <+>) <$> kw "distinct"
        exprs -> do
            ls <- nonEmptyLocs (concatMap locsNode exprs)
            i <- tokAt (minimum ls)
            let findOn j = do
                    w <- tokL j
                    if
                        | w == "on" -> pure j
                        | w == "(" -> findOn (j - 1)
                        | otherwise -> unsupported "a distinct on pgfmt cannot read"
            on <- findOn (i - 1)
            _ <- expectParen (on + 1)
            ds <- traverse (top >=> expr MP) exprs
            d <- kw "distinct on"
            pure (if mode == MP then d <> " (" <> commaJoin ds <> ")" else sel <+> d <+> "(" <> commaJoin ds <> ")")


targeting :: Pg.SelectStmt -> R D
targeting s = do
    prefix <- targetPrefix MR s
    case s ^. Pg.targetList of
        [] -> pure prefix
        [single] -> do
            forces <- targetForcesMultiLine single
            q <- targetFirst single
            commented <- maybe (pure False) (fmap (not . null) . gapBefore) q
            if forces || commented then blockList prefix targetElDoc targetFirst [single] else (prefix <+>) <$> targetElDoc single
        items -> blockList prefix targetElDoc targetFirst items


-- | A target's first token.
targetFirst :: Pg.Node -> R (Maybe Int)
targetFirst n = case node n of
    Just (Pg.Node'ResTarget rt) | rt ^. Pg.location >= 0 -> Just <$> tokAt (fromIntegral (rt ^. Pg.location))
    _ -> pure Nothing


targetForcesMultiLine :: Pg.Node -> R Bool
targetForcesMultiLine n = case node n of
    Just (Pg.Node'ResTarget rt) | Just v <- rt ^. Pg.maybe'val -> top v >>= isTopLevelCase
    _ -> pure False


-- | "PgFmt.Render"'s @renderBlockListWith@; with each item's first token,
-- for the comments before it.
blockList :: D -> (a -> R D) -> (a -> R (Maybe Int)) -> [a] -> R D
blockList prefix item first items = do
    rows <- zipWithM row [(0 :: Int) ..] items
    pure (prefix <> mconcat rows)
  where
    row i x = do
        b <- first x >>= brkAt "    "
        d <- item x
        pure (b <> (if i == 0 then "    " else "  , ") <> d)


-- | A target: its expression and alias.
resTarget :: Pg.Node -> R (Pg.ResTarget, E)
resTarget n = case node n of
    Just (Pg.Node'ResTarget rt) -> do
        v <- req "a target" (rt ^. Pg.maybe'val)
        e <- top v
        pure (rt, e)
    _ -> unsupported "a target pgfmt cannot read"


targetWith :: (E -> R D) -> Pg.Node -> R D
targetWith render n = do
    (rt, e) <- resTarget n
    d <- render e
    a <- targetAlias rt
    pure (d <> a)


-- | "PgFmt.Render"'s @renderTargetEl@, @renderTargetElDoc@, and
-- postgresql-syntax's own.
targetEl, targetElDoc, targetElP :: Pg.Node -> R D
targetEl = targetWith (expr MR)
targetElDoc = targetWith exprDoc
targetElP = targetWith (expr MP)


-- | A target's alias, with @as@ if the source has it.
targetAlias :: Pg.ResTarget -> R D
targetAlias rt
    | T.null name = pure mempty
    | otherwise = do
        v <- req "a target" (rt ^. Pg.maybe'val)
        ls <- nonEmptyLocs (locsNode v)
        s <- tokAt (fromIntegral (rt ^. Pg.location))
        e <- tokAt (maximum ls)
        d0 <- depthBetween s (e + 1)
        found <- find (e + 1) d0
        hasAs <- tokIs (found - 1) "as"
        a <- ident name
        asK <- kw "as"
        pure (" " <> (if hasAs then asK <> " " else mempty) <> a)
  where
    name = rt ^. Pg.name
    find i d = do
        t <- tokM i
        case t of
            Nothing -> unsupported "an alias pgfmt cannot find"
            Just tk
                | d == 0, identText tk == Just name -> do
                    -- An alias ends the target; a column of the same name in
                    -- @t.name@ does not.
                    next <- tokM (i + 1)
                    let ends = case next of
                            Nothing -> True
                            Just n -> n.text `elem` [",", ")", ";"] || (n.isKeyword && T.toLower n.text `elem` clauseWords)
                    if ends then pure i else find (i + 1) d
                | tk.text `elem` ["(", "["] -> find (i + 1) (d + 1)
                | tk.text `elem` [")", "]"] -> if d == 0 then unsupported "an alias pgfmt cannot find" else find (i + 1) (d - 1)
                | otherwise -> find (i + 1) d


-- | Keywords that can follow a select or returning list.
clauseWords :: [Text]
clauseWords = ["from", "into", "where", "group", "having", "window", "order", "limit", "offset", "fetch", "for", "union", "intersect", "except", "returning", "on"]


-- | The identifier a token spells, if it is one.
identText :: Tok -> Maybe Text
identText tk
    | tk.kind == Pg.IDENT, Just q <- T.stripPrefix "\"" tk.text = Just (T.replace "\"\"" "\"" (T.dropEnd 1 q))
    | tk.kind == Pg.IDENT || tk.isKeyword = Just (foldCase tk.text)
    | otherwise = Nothing


depthBetween :: Int -> Int -> R Int
depthBetween a b = sum <$> forM [a .. b - 1] \i -> do
    w <- tokL i
    pure if w `elem` ["(", "["] then 1 else if w `elem` [")", "]"] then -1 else 0


-- | "PgFmt.Render"'s @renderSelectWithParensMaybeWrap@.
selMaybeWrap :: Sel -> R D
selMaybeWrap sel = do
    inline <- selInline sel
    if len inline > subqueryInlineThreshold then selWrapped sel else pure inline


selWrapped :: Sel -> R D
selWrapped (Sel s n) = do
    inner <- if n <= 1 then selNoParens s else selWrapped (Sel s (n - 1))
    pure ("(" <> nest indentSize (hardline <> inner) <> hardline <> ")")


-- | "PgFmt.Render"'s @renderSelectWithParensInline@.
selInline :: Sel -> R D
selInline (Sel s n) = do
    inner <- if n <= 1 then selNoParensInline s else selInline (Sel s (n - 1))
    pure ("(" <> inner <> ")")


selNoParensInline :: Pg.SelectStmt -> R D
selNoParensInline s = do
    w <- traverse withP (s ^. Pg.maybe'withClause)
    c <- selClauseInline s
    so <- if null (s ^. Pg.sortClause) then pure Nothing else Just <$> sortClause (s ^. Pg.sortClause)
    l <- selectLimit False s
    lk <- locking s
    pure (spaceJoin (catMaybes [w, Just c, so, l, lk]))


selClauseInline :: Pg.SelectStmt -> R D
selClauseInline s
    | s ^. Pg.op /= Pg.SETOP_NONE = do
        (l, lp, r, rp) <- operands s
        ld <- if lp == 0 then selClauseInline l else selInline (Sel l lp)
        (op, _) <- setOperator s r rp
        rd <- if rp == 0 then selClauseInline r else selInline (Sel r rp)
        pure (ld <+> op <+> rd)
    | not (null (s ^. Pg.valuesLists)) = valuesP s
    | otherwise = do
        when (isJust (s ^. Pg.maybe'intoClause)) (unsupported "select into")
        prefix <- targetPrefix MR s
        targets <- traverse targetEl (s ^. Pg.targetList)
        let t = if null targets then prefix else prefix <+> commaJoin targets
        f <-
            if null (s ^. Pg.fromClause)
                then pure Nothing
                else do
                    k <- kw "from"
                    ds <- traverse tableRefInline (s ^. Pg.fromClause)
                    pure (Just (k <+> commaJoin ds))
        w <- forM (s ^. Pg.maybe'whereClause) \x -> do
            k <- kw "where"
            d <- top x >>= expr MR
            pure (k <+> d)
        g <- groupClause s
        h <- forM (s ^. Pg.maybe'havingClause) \x -> do
            k <- kw "having"
            d <- top x >>= expr MR
            pure (k <+> d)
        wi <- windowClause s
        pure (spaceJoin (catMaybes [Just t, f, w, g, h, wi]))


-- | A select as postgresql-syntax writes it.
selP :: Sel -> R D
selP (Sel s n) = do
    inner <- if n <= 1 then selNoParensP s else selP (Sel s (n - 1))
    pure ("(" <> inner <> ")")


selNoParensP :: Pg.SelectStmt -> R D
selNoParensP s = do
    w <- traverse withP (s ^. Pg.maybe'withClause)
    c <- selClauseP s
    so <- if null (s ^. Pg.sortClause) then pure Nothing else Just <$> sortClause (s ^. Pg.sortClause)
    l <- selectLimit False s
    lk <- locking s
    pure (spaceJoin (catMaybes [w, Just c, so, l, lk]))


selClauseP :: Pg.SelectStmt -> R D
selClauseP s
    | s ^. Pg.op /= Pg.SETOP_NONE = do
        (l, lp, r, rp) <- operands s
        ld <- if lp == 0 then selClauseP l else selP (Sel l lp)
        (op, _) <- setOperator s r rp
        rd <- if rp == 0 then selClauseP r else selP (Sel r rp)
        pure (ld <+> op <+> rd)
    | not (null (s ^. Pg.valuesLists)) = valuesP s
    | otherwise = do
        when (isJust (s ^. Pg.maybe'intoClause)) (unsupported "select into")
        sel <- kw "select"
        targets <- traverse targetElP (s ^. Pg.targetList)
        quantifier <- case s ^. Pg.distinctClause of
            [] -> do
                p <- targetPrefix MP s
                pure if renderToText p == renderToText sel then Nothing else Just p
            _ -> Just <$> targetPrefixP s
        let t = case (quantifier, targets) of
                (Nothing, []) -> Nothing
                (Nothing, _) -> Just (commaJoin targets)
                (Just q, []) -> Just q
                (Just q, _) -> Just (q <+> commaJoin targets)
        f <-
            if null (s ^. Pg.fromClause)
                then pure Nothing
                else do
                    k <- kw "from"
                    ds <- traverse tableRefP (s ^. Pg.fromClause)
                    pure (Just (k <+> commaJoin ds))
        w <- forM (s ^. Pg.maybe'whereClause) \x -> do
            k <- kw "where"
            d <- top x >>= expr MP
            pure (k <+> d)
        g <- groupClause s
        h <- forM (s ^. Pg.maybe'havingClause) \x -> do
            k <- kw "having"
            d <- top x >>= expr MP
            pure (k <+> d)
        wi <- windowClause s
        pure (spaceJoin (sel : catMaybes [t, f, w, g, h, wi]))
  where
    -- postgresql-syntax's @Targeting@ without the @select@: @all@,
    -- @distinct@, @distinct on (...)@.
    targetPrefixP st = case st ^. Pg.distinctClause of
        [x] | isNothing (node x) -> kw "distinct"
        _ -> do
            d <- targetPrefix MP st
            pure d


groupClause :: Pg.SelectStmt -> R (Maybe D)
groupClause s
    | null (s ^. Pg.groupClause) = pure Nothing
    | s ^. Pg.groupDistinct = unsupported "group by distinct"
    | otherwise = do
        k <- kw "group by"
        ds <- traverse groupItem (s ^. Pg.groupClause)
        positions <- commentedGroup
        case positions of
            -- With comments, one item per line, as a select's targets.
            Just firsts -> Just <$> blockList k (pure . fst) (pure . Just . snd) (zip ds firsts)
            Nothing -> pure (Just (k <+> commaJoin ds))
  where
    items = s ^. Pg.groupClause
    commentedGroup = do
        start <- maybe (pure Nothing) (clauseAt [["group", "by"]]) (listToMaybe items)
        end <- maybe (pure Nothing) spanOf (listToMaybe (reverse items))
        case (start, end) of
            (Just g, Just (_, e)) -> do
                commented <- commentsBetween (g + 1) e
                if commented then listFirsts (g + 1) (e + 1) (length items) else pure Nothing
            _ -> pure Nothing
    groupItem n = case node n of
        Just (Pg.Node'GroupingSet g) -> do
            at <- tokAt (fromIntegral (g ^. Pg.location))
            case g ^. Pg.kind of
                Pg.GROUPING_SET_EMPTY -> do
                    _ <- expectParen at
                    txt "()"
                k | k `elem` [Pg.GROUPING_SET_ROLLUP, Pg.GROUPING_SET_CUBE] -> do
                    open <- nextParen at
                    _ <- expectParen open
                    ds <- traverse (top >=> expr MP) (g ^. Pg.content)
                    w <- kw (if k == Pg.GROUPING_SET_ROLLUP then "rollup" else "cube")
                    pure (w <> " (" <> commaJoin ds <> ")")
                Pg.GROUPING_SET_SETS -> do
                    open <- nextParen at
                    _ <- expectParen open
                    ds <- traverse groupItem (g ^. Pg.content)
                    w <- kw "grouping sets"
                    pure (w <> " (" <> commaJoin ds <> ")")
                _ -> unsupported "a grouping set pgfmt cannot read"
        _ -> top n >>= expr MP


windowClause :: Pg.SelectStmt -> R (Maybe D)
windowClause s
    | null (s ^. Pg.windowClause) = pure Nothing
    | otherwise = do
        k <- kw "window"
        ds <- forM (s ^. Pg.windowClause) \n -> case node n of
            Just (Pg.Node'WindowDef w) -> do
                at <- tokAt (fromIntegral (w ^. Pg.location))
                open <- nextParen at
                _ <- expectParen open
                name <- ident (w ^. Pg.name)
                asK <- kw "as"
                spec <- windowSpec w
                pure (name <+> asK <+> spec)
            _ -> unsupported "a window pgfmt cannot read"
        pure (Just (k <+> commaJoin ds))


sortClause :: [Pg.Node] -> R D
sortClause items = do
    k <- kw "order by"
    ds <- forM items \n -> case node n of
        Just (Pg.Node'SortBy sb) -> do
            e <- req "a sort" (sb ^. Pg.maybe'node) >>= top >>= expr MP
            dir <- case sb ^. Pg.sortbyDir of
                Pg.SORTBY_ASC -> Just <$> kw "asc"
                Pg.SORTBY_DESC -> Just <$> kw "desc"
                Pg.SORTBY_USING -> do
                    ops <- strsOf (sb ^. Pg.useOp)
                    case ops of
                        [op] -> do
                            u <- kw "using"
                            pure (Just (u <+> pretty op))
                        _ -> unsupported "a sort operator pgfmt cannot read"
                _ -> pure Nothing
            nulls <- case sb ^. Pg.sortbyNulls of
                Pg.SORTBY_NULLS_FIRST -> Just <$> kw "nulls first"
                Pg.SORTBY_NULLS_LAST -> Just <$> kw "nulls last"
                _ -> pure Nothing
            pure (spaceJoin (e : catMaybes [dir, nulls]))
        _ -> unsupported "a sort pgfmt cannot read"
    pure (k <+> commaJoin ds)


-- | @limit@ and @offset@, in the source's order; on separate lines in a
-- multi-line select.
selectLimit :: Bool -> Pg.SelectStmt -> R (Maybe D)
selectLimit multiLine s = do
    unless (s ^. Pg.limitOption `elem` [Pg.LIMIT_OPTION_DEFAULT, Pg.LIMIT_OPTION_COUNT]) (unsupported "fetch with ties")
    l <- traverse limitPart (s ^. Pg.maybe'limitCount)
    o <- traverse offsetPart (s ^. Pg.maybe'limitOffset)
    let sep a b = if multiLine then a <> hardline <> b else a <+> b
    pure case (l, o) of
        (Nothing, Nothing) -> Nothing
        (Just (a, _), Nothing) -> Just a
        (Nothing, Just (b, _)) -> Just b
        (Just (a, la), Just (b, lb)) -> Just (if la < lb then sep a b else sep b a)
  where
    limitPart n = do
        e <- top n
        start <- clauseStart n e
        w <- tokL (start - 1)
        unless (w == "limit") (unsupported "fetch first")
        k <- kw "limit"
        v <- case node n of
            Just (Pg.Node'AConst c) | c ^. Pg.isnull -> do
                src <- tokAt (fromIntegral (c ^. Pg.location)) >>= tokL
                kw (if src == "all" then "all" else "null")
            _ -> expr MP e
        pure (k <+> v, start)
    offsetPart n = do
        e <- top n
        start <- clauseStart n e
        w <- tokL (start - 1)
        unless (w == "offset") (unsupported "an offset pgfmt cannot read")
        ls <- nonEmptyLocs (locsNode n)
        after <- tokAt (maximum ls) >>= \i -> tokL (i + 1)
        when (after `elem` ["row", "rows"]) (unsupported "offset ... rows")
        k <- kw "offset"
        v <- expr MP e
        pure (k <+> v, start)
    clauseStart n e = do
        ls <- nonEmptyLocs (locsNode n)
        i <- tokAt (minimum ls)
        pure (i - e.lv)


locking :: Pg.SelectStmt -> R (Maybe D)
locking s
    | null (s ^. Pg.lockingClause) = pure Nothing
    | otherwise = do
        ds <- forM (s ^. Pg.lockingClause) \n -> case node n of
            Just (Pg.Node'LockingClause lc) -> do
                strength <- kw case lc ^. Pg.strength of
                    Pg.LCS_FORKEYSHARE -> "for key share"
                    Pg.LCS_FORSHARE -> "for share"
                    Pg.LCS_FORNOKEYUPDATE -> "for no key update"
                    _ -> "for update"
                rels <-
                    if null (lc ^. Pg.lockedRels)
                        then pure Nothing
                        else do
                            k <- kw "of"
                            ds <- forM (lc ^. Pg.lockedRels) \r -> case node r of
                                Just (Pg.Node'RangeVar rv) -> relationName rv
                                _ -> unsupported "a locked relation pgfmt cannot read"
                            pure (Just (k <+> commaJoin ds))
                wait <- case lc ^. Pg.waitPolicy of
                    Pg.LockWaitSkip -> Just <$> kw "skip locked"
                    Pg.LockWaitError -> Just <$> kw "nowait"
                    _ -> pure Nothing
                pure (spaceJoin (catMaybes [Just strength, rels, wait]))
            _ -> unsupported "a locking clause pgfmt cannot read"
        pure (Just (spaceJoin ds))


-- | Each row's items, with its parentheses placed.
valuesRows :: Pg.SelectStmt -> R [[Pg.Node]]
valuesRows s = map snd <$> valuesRowsAt s


-- | Each row's opening parenthesis and items, its parentheses placed.
valuesRowsAt :: Pg.SelectStmt -> R [(Int, [Pg.Node])]
valuesRowsAt s = do
    when (isJust (s ^. Pg.maybe'withClause)) (unsupported "values with a with clause")
    rows <- forM (s ^. Pg.valuesLists) \r -> case node r of
        Just (Pg.Node'List l) -> pure (l ^. Pg.items)
        _ -> unsupported "a values row pgfmt cannot read"
    first <- selectFirst s
    let place _ [] = pure []
        place i (row : rest) = do
            close <- expectParen i
            unless (null rest) do
                comma <- tokIs (close + 1) ","
                unless comma (unsupported "values rows pgfmt cannot read")
            ((i, row) :) <$> place (close + 2) rest
    place (first + 1) rows


-- | "PgFmt.Render"'s @renderValuesClause@.
valuesClause :: Pg.SelectStmt -> R D
valuesClause s = do
    rows <- valuesRowsAt s
    k <- kw "values"
    let lastIdx = length rows - 1
    laid <- forM rows \(open, items) -> do
        ds <- traverse (top >=> expr MR) items
        close <- matchOf open >>= req "a values row"
        commented <- commentsBetween open close
        pure (open, close, ds, commented)
    -- Each row on one line when every row fits and none has a comment in
    -- it; otherwise one value per line, as "PgFmt.Render" always does.
    let oneLine = all (\(_, _, ds, commented) -> not commented && len (inlineRow ds) <= valuesRowInlineThreshold) laid
    docs <- forM (zip [(0 :: Int) ..] laid) \(i, (open, close, ds, commented)) -> do
        bracketed <-
            if commented
                then do
                    firsts <- listFirsts open close (length ds)
                    bracketListAt ((,close) <$> firsts) ds
                else pure case ds of
                    [_] -> inlineRow ds
                    _ | oneLine -> inlineRow ds
                    _ -> bracketList ds
        b <- brk mempty open
        -- A blank line the source has before a row, after the first, is
        -- kept: it separates groups of rows.
        blank <- asks (IS.member open . (.blankLineBefore))
        let gap = if blank && i > 0 then hardline else mempty
        pure (b <> gap <> if i < lastIdx then bracketed <> "," else bracketed)
    pure (k <> nest indentSize (mconcat docs))
  where
    inlineRow ds = "(" <> commaJoin ds <> ")"


-- | The longest @values@ row that goes on one line.
valuesRowInlineThreshold :: Int
valuesRowInlineThreshold = 100


valuesP :: Pg.SelectStmt -> R D
valuesP s = do
    rows <- valuesRows s
    k <- kw "values"
    docs <- forM rows \items -> do
        ds <- traverse (top >=> expr MP) items
        pure ("(" <> commaJoin ds <> ")")
    pure (k <+> commaJoin docs)


bracketList :: [D] -> D
bracketList = \case
    [] -> "()"
    (x : xs) -> vsep (("( " <> x) : map (", " <>) xs ++ [")"])


-- | 'bracketList', with the comments before each item and before the
-- closing parenthesis, if each item's first token and the parenthesis are
-- known.
bracketListAt :: Maybe ([Int], Int) -> [D] -> R D
bracketListAt Nothing ds = pure (bracketList ds)
bracketListAt (Just (firsts, close)) ds = case zip firsts ds of
    [] -> pure "()"
    (q0, x) : rest -> do
        -- After the opening parenthesis, a comment goes before the first
        -- item, on its own line.
        cs <- gapBefore q0
        let before = mconcat [raw c.ctext <> hardline | c <- cs]
        items <- forM rest \(q, d) -> do
            b <- brk "  " q
            pure (b <> ", " <> d)
        end <- brk "  " close
        pure (before <> "( " <> x <> mconcat items <> end <> ")")


withClause :: Pg.WithClause -> R D
withClause w = do
    k <- kw (if w ^. Pg.recursive then "with recursive" else "with")
    let ctes = w ^. Pg.ctes
        lastIdx = length ctes - 1
    docs <- forM (zip [0 :: Int ..] ctes) \(i, c) -> do
        d <- cte (i == lastIdx) c
        at <- case node c of
            Just (Pg.Node'CommonTableExpr x) | x ^. Pg.location >= 0 -> Just <$> tokAt (fromIntegral (x ^. Pg.location))
            _ -> pure Nothing
        pure (at, if i == 0 then k <> " " <> d else d)
    linesAt mempty docs


-- | A common table expression's header (@name (cols) as materialized@) and
-- body, its own parenthesis placed.
cteParts :: Pg.Node -> R (D, Pg.Node)
cteParts n = case node n of
    Just (Pg.Node'CommonTableExpr c) -> do
        when (isJust (c ^. Pg.maybe'searchClause) || isJust (c ^. Pg.maybe'cycleClause)) (unsupported "search or cycle")
        at <- tokAt (fromIntegral (c ^. Pg.location))
        name <- ident (c ^. Pg.ctename)
        cols <-
            if null (c ^. Pg.aliascolnames)
                then pure mempty
                else do
                    _ <- expectParen (at + 1)
                    cs <- strsOf (c ^. Pg.aliascolnames) >>= traverse ident
                    pure (" (" <> commaJoin cs <> ")")
        mat <- case c ^. Pg.ctematerialized of
            Pg.CTEMaterializeAlways -> Just <$> kw "materialized"
            Pg.CTEMaterializeNever -> Just <$> kw "not materialized"
            _ -> pure Nothing
        open <- nextParen (at + 1 + (if null (c ^. Pg.aliascolnames) then 0 else 1))
        -- The body's parenthesis: after the column list, if any.
        open' <-
            if null (c ^. Pg.aliascolnames)
                then pure open
                else matchOf (at + 1) >>= req "a column list" >>= \close -> nextParen (close + 1)
        _ <- expectParen open'
        asK <- kw "as"
        q <- req "a common table expression" (c ^. Pg.maybe'ctequery)
        pure (name <> cols <> " " <> asK <> " " <> maybe mempty (<> " ") mat, q)
    _ -> unsupported "a common table expression pgfmt cannot read"


cte :: Bool -> Pg.Node -> R D
cte isLast n = do
    (header, q) <- cteParts n
    body <- preparable q
    pure (header <> "(" <> hardline <> indent indentSize body <> hardline <> (if isLast then ")" else "),"))


withP :: Pg.WithClause -> R D
withP w = do
    k <- kw (if w ^. Pg.recursive then "with recursive" else "with")
    docs <- forM (w ^. Pg.ctes) \c -> do
        (header, q) <- cteParts c
        body <- case node q of
            Just (Pg.Node'SelectStmt st) -> do
                n <- selectParens 0 st
                if n == 0 then selNoParensP st else selP (Sel st n)
            _ -> unsupported "a data-modifying common table expression inside an expression"
        pure (header <> "(" <> body <> ")")
    pure (k <+> commaJoin docs)


----------------------------------------------------------------------
-- From.
----------------------------------------------------------------------

-- | "PgFmt.Render"'s @renderFromClause@: on one line, unless a join (or a
-- long call or subquery) puts each item on its own.
fromClause :: [Pg.Node] -> R D
fromClause items = do
    multi <- anyM forcesMultiLine items
    if multi
        then do
            let lastIdx = length items - 1
            docs <- forM (zip [0 :: Int ..] items) \(i, item) -> do
                d <- tableRef (i == 0) item
                pure (if i < lastIdx then d <> "," else d)
            pure (vsep docs)
        else do
            k <- kw "from"
            ds <- traverse tableRefInline items
            pure (k <+> commaJoin ds)
  where
    forcesMultiLine n = case node n of
        Just (Pg.Node'JoinExpr _) -> pure True
        Just (Pg.Node'RangeFunction rf) -> isJust <$> wrapEligibleFuncTable rf
        Just (Pg.Node'RangeSubselect rs) -> do
            sel <- rangeSubselectSel rs
            d <- selInline sel
            pure (len d > subqueryInlineThreshold)
        _ -> pure False


-- | A set-returning call in a from list, when it wraps.
wrapEligibleFuncTable :: Pg.RangeFunction -> R (Maybe Pg.FuncCall)
wrapEligibleFuncTable rf = do
    f <- rangeFunctionCall rf
    case f of
        Just call | funcAppArgCount call >= funcWrapThreshold -> do
            plain <- classifyCall call
            case plain of
                Plain _ | not (hasExtras call) -> do
                    e <- rangeFunctionNode rf >>= top
                    d <- funcApplication MR e call
                    pure if len d > inlineLengthThreshold then Just call else Nothing
                _ -> pure Nothing
        _ -> pure Nothing


withFuncCall :: Pg.Node -> Pg.FuncCall -> Pg.Node
withFuncCall n f = n & Pg.funcCall .~ f


-- | The function of a from-list call, if it is a call.
rangeFunctionCall :: Pg.RangeFunction -> R (Maybe Pg.FuncCall)
rangeFunctionCall rf = do
    when (rf ^. Pg.isRowsfrom) (unsupported "rows from")
    case rf ^. Pg.functions of
        [item] | Just (Pg.Node'List l) <- node item -> case l ^. Pg.items of
            fn : rest | all (isNothing . node) rest -> pure case node fn of
                Just (Pg.Node'FuncCall f) -> Just f
                _ -> Nothing
            _ -> unsupported "a from-list function pgfmt cannot read"
        _ -> unsupported "a from-list function pgfmt cannot read"


rangeFunctionNode :: Pg.RangeFunction -> R Pg.Node
rangeFunctionNode rf = case rf ^. Pg.functions of
    [item] | Just (Pg.Node'List l) <- node item, fn : _ <- l ^. Pg.items -> pure fn
    _ -> unsupported "a from-list function pgfmt cannot read"


rangeSubselectSel :: Pg.RangeSubselect -> R Sel
rangeSubselectSel rs = do
    q <- req "a subquery" (rs ^. Pg.maybe'subquery)
    st <- case node q of
        Just (Pg.Node'SelectStmt st) -> pure st
        _ -> unsupported "a from-list subquery that is not a select"
    n <- selectParens 0 st
    when (n < 1) (unsupported "a from-list subquery without parentheses")
    pure (Sel st n)


-- | The first token of a table reference.
tableRefFirst :: Pg.Node -> R Int
tableRefFirst n = case node n of
    Just (Pg.Node'RangeVar rv) -> tokAt (fromIntegral (rv ^. Pg.location)) >>= lateral False
    Just (Pg.Node'RangeTableSample ts) -> req "a table sample" (ts ^. Pg.maybe'relation) >>= tableRefFirst
    Just (Pg.Node'RangeSubselect rs) -> do
        q <- req "a subquery" (rs ^. Pg.maybe'subquery)
        st <- case node q of
            Just (Pg.Node'SelectStmt st) -> pure st
            _ -> unsupported "a from-list subquery that is not a select"
        f <- selectFirst st
        k <- selectParensPeek st
        lateral (rs ^. Pg.lateral) (f - k)
    Just (Pg.Node'RangeFunction rf) -> do
        fn <- rangeFunctionNode rf
        ls <- nonEmptyLocs (locsNode fn)
        tokAt (minimum ls) >>= lateral (rf ^. Pg.lateral)
    Just (Pg.Node'JoinExpr j) -> do
        l <- req "a join" (j ^. Pg.maybe'larg)
        r <- req "a join" (j ^. Pg.maybe'rarg)
        f <- tableRefFirst l
        rf <- tableRefFirst r
        extendOver f rf
    _ -> unsupported (nodeName n)
  where
    lateral isLateral i = do
        w <- tokL (i - 1)
        pure if isLateral && w == "lateral" then i - 1 else i


-- | The parentheses around a join, placing them.
joinParens :: Pg.JoinExpr -> Pg.Node -> R Int
joinParens _ n = do
    ls0 <- nonEmptyLocs (locsNode n)
    remembered (3, minimum ls0, maximum ls0, length ls0) (joinParens' n)


joinParens' :: Pg.Node -> R Int
joinParens' n = do
    f <- tableRefFirst n
    ls <- nonEmptyLocs (locsNode n)
    let set = IS.fromList ls
    maxTok <- tokAt (maximum ls)
    let go p k = do
            isParen <- tokIs p "("
            placed <- isConsumed p
            mm <- matchOf p
            case mm of
                Just m | isParen, not placed, m > maxTok -> do
                    cov <- covers p m set
                    if cov then consume p >> go (p - 1) (k + 1) else pure k
                _ -> pure k
    go (f - 1) 0


-- | A relation's name.
relationName :: Pg.RangeVar -> R D
relationName rv = do
    unless (rv ^. Pg.inh) (unsupported "only")
    at <- tokAt (fromIntegral (rv ^. Pg.location))
    qualNameAt at (filter (not . T.null) [rv ^. Pg.catalogname, rv ^. Pg.schemaname, rv ^. Pg.relname])


-- | A relation with its alias, as postgresql-syntax writes it.
relation :: Pg.RangeVar -> R D
relation rv = do
    name <- relationName rv
    at <- tokAt (fromIntegral (rv ^. Pg.location))
    let parts = length (filter (not . T.null) [rv ^. Pg.catalogname, rv ^. Pg.schemaname, rv ^. Pg.relname])
        after = at + 2 * parts - 1
    star <- tokIs after "*"
    when star (unsupported "a relation with *")
    a <- alias (rv ^. Pg.maybe'alias) after
    pure (name <> a)


-- | @t tablesample system (10) repeatable (1)@, as postgresql-syntax
-- writes it.
tableSample :: Pg.RangeTableSample -> R D
tableSample ts = do
    rel <- case ts ^. Pg.maybe'relation >>= node of
        Just (Pg.Node'RangeVar rv) -> relation rv
        _ -> unsupported "a table sample pgfmt cannot read"
    at <- tokAt (fromIntegral (ts ^. Pg.location))
    open <- nextParen at
    close <- expectParen open
    k <- kw "tablesample"
    method <- strsOf (ts ^. Pg.method) >>= qualName
    args <- traverse (top >=> expr MP) (ts ^. Pg.args)
    rep' <- case ts ^. Pg.maybe'repeatable of
        Nothing -> pure mempty
        Just r -> do
            ropen <- nextParen (close + 1)
            _ <- expectParen ropen
            rk <- kw "repeatable"
            rd <- top r >>= expr MP
            pure (" " <> rk <> " (" <> rd <> ")")
    pure (rel <+> k <+> method <> " (" <> commaJoin args <> ")" <> rep')


-- | An alias starting at this token, with a space before it.
alias :: Maybe Pg.Alias -> Int -> R D
alias Nothing _ = pure mempty
alias (Just a) i = do
    hasAs <- tokIs i "as"
    let nameTok = if hasAs then i + 1 else i
    t <- tokM nameTok
    unless ((t >>= identText) == Just (a ^. Pg.aliasname)) (unsupported "an alias pgfmt cannot find")
    name <- ident (a ^. Pg.aliasname)
    asK <- kw "as"
    cols <-
        if null (a ^. Pg.colnames)
            then pure mempty
            else do
                _ <- expectParen (nameTok + 1)
                cs <- strsOf (a ^. Pg.colnames) >>= traverse ident
                pure (" (" <> commaJoin cs <> ")")
    pure (" " <> (if hasAs then asK <> " " else mempty) <> name <> cols)


-- | "PgFmt.Render"'s @renderFuncAliasClauseDoc@: more than three columns go
-- on their own lines.
funcAlias :: Maybe Pg.Alias -> Int -> R D
funcAlias Nothing _ = pure mempty
funcAlias (Just a) i
    | length (a ^. Pg.colnames) > 3 = do
        hasAs <- tokIs i "as"
        let nameTok = if hasAs then i + 1 else i
        name <- ident (a ^. Pg.aliasname)
        asK <- kw "as"
        _ <- expectParen (nameTok + 1)
        cs <- strsOf (a ^. Pg.colnames) >>= traverse ident
        pure (" " <> (if hasAs then asK <> " " else mempty) <> name <> hardline <> indent indentSize (bracketList cs))
    | otherwise = alias (Just a) i


-- | A from-list call's alias, maybe with column definitions
-- (@as t (a int, b text)@), starting at this token.
funcAliasClause :: Mode -> Pg.RangeFunction -> Int -> R D
funcAliasClause mode rf i
    | null (rf ^. Pg.coldeflist) = (if mode == MP then alias else funcAlias) (rf ^. Pg.maybe'alias) i
    | otherwise = do
        hasAs <- tokIs i "as"
        asK <- kw "as"
        (name, open) <- case rf ^. Pg.maybe'alias of
            Nothing -> do
                unless hasAs (unsupported "a column definition list pgfmt cannot read")
                pure (Nothing, i + 1)
            Just a -> do
                unless (null (a ^. Pg.colnames)) (unsupported "an alias with both columns and definitions")
                let nameTok = if hasAs then i + 1 else i
                t <- tokM nameTok
                unless ((t >>= identText) == Just (a ^. Pg.aliasname)) (unsupported "an alias pgfmt cannot find")
                n <- ident (a ^. Pg.aliasname)
                pure (Just n, nameTok + 1)
        _ <- expectParen open
        defs <- forM (rf ^. Pg.coldeflist) \n -> case node n of
            Just (Pg.Node'ColumnDef cd) -> do
                when (isJust (cd ^. Pg.maybe'collClause) || not (null (cd ^. Pg.constraints))) (unsupported "a column definition with options")
                c <- ident (cd ^. Pg.colname)
                t <- req "a column definition" (cd ^. Pg.maybe'typeName) >>= typeName
                pure (c <> " " <> t)
            _ -> unsupported "a column definition pgfmt cannot read"
        pure (" " <> spaceJoin ([asK | hasAs] ++ toList name) <> " (" <> commaJoin defs <> ")")


-- | The token after a from-list call (and @with ordinality@).
afterFunction :: Pg.RangeFunction -> R Int
afterFunction rf = do
    fn <- rangeFunctionNode rf
    ls <- nonEmptyLocs (locsNode fn)
    at <- tokAt (minimum ls)
    open <- nextParen at
    close <- matchOf open >>= req "a call"
    pure (if rf ^. Pg.ordinality then close + 3 else close + 1)


lateralD :: Bool -> R D
lateralD False = pure mempty
lateralD True = (<> " ") <$> kw "lateral"


ordinalityD :: Pg.RangeFunction -> R D
ordinalityD rf = if rf ^. Pg.ordinality then (" " <>) <$> kw "with ordinality" else pure mempty


-- | A from-list call laid out by 'MR' or 'MP'.
funcTable :: Mode -> Pg.RangeFunction -> R D
funcTable mode rf = do
    fn <- rangeFunctionNode rf
    e <- top fn
    d <- case node fn of
        Just (Pg.Node'FuncCall f) | not (hasExtras f) -> do
            call <- classifyCall f
            case call of
                Plain _ -> funcApplication mode e f
                AtTimeZone _ _ -> unsupported "at time zone in a from list"
                _ -> funcCall mode e f
        Just (Pg.Node'CoalesceExpr _) -> expr mode e
        Just (Pg.Node'MinMaxExpr _) -> expr mode e
        Just (Pg.Node'SqlvalueFunction _) -> expr mode e
        _ -> unsupported "a from-list function pgfmt cannot read"
    o <- ordinalityD rf
    pure (d <> o)


subselectAlias :: Pg.RangeSubselect -> Sel -> R D
subselectAlias rs sel = do
    f <- selectFirst sel.stmt
    close <- matchOf (f - sel.parens) >>= req "a subquery"
    alias (rs ^. Pg.maybe'alias) (close + 1)


-- | "PgFmt.Render"'s @renderTableRef@.
tableRef :: Bool -> Pg.Node -> R D
tableRef isFirst n = do
    pfx <- if isFirst then (<> " ") <$> kw "from" else pure mempty
    (pfx <>) <$> case node n of
        Just (Pg.Node'RangeVar rv) -> relation rv
        Just (Pg.Node'RangeTableSample ts) -> tableSample ts
        Just (Pg.Node'RangeFunction rf) -> do
            wrap <- wrapEligibleFuncTable rf
            l <- lateralD (rf ^. Pg.lateral)
            after <- afterFunction rf
            a <- funcAliasClause MR rf after
            case wrap of
                Just f -> do
                    fn <- rangeFunctionNode rf
                    e <- top fn
                    d <- funcApplicationWrapped e f
                    o <- ordinalityD rf
                    pure (l <> d <> o <> a)
                Nothing -> do
                    d <- funcTable MR rf
                    pure (l <> d <> a)
        Just (Pg.Node'RangeSubselect rs) -> do
            sel <- rangeSubselectSel rs
            l <- lateralD (rs ^. Pg.lateral)
            d <- selMaybeWrap sel
            a <- subselectAlias rs sel
            pure (l <> d <> a)
        Just (Pg.Node'JoinExpr j) -> do
            k <- joinParens j n
            joinedTable j k
        _ -> unsupported (nodeName n)


-- | "PgFmt.Render"'s @renderTableRefInline@.
tableRefInline :: Pg.Node -> R D
tableRefInline n = case node n of
    Just (Pg.Node'RangeVar rv) -> relation rv
    Just (Pg.Node'RangeTableSample ts) -> tableSample ts
    Just (Pg.Node'RangeFunction rf) -> do
        l <- lateralD (rf ^. Pg.lateral)
        d <- funcTable MR rf
        after <- afterFunction rf
        a <- funcAliasClause MR rf after
        pure (l <> d <> a)
    Just (Pg.Node'RangeSubselect rs) -> do
        sel <- rangeSubselectSel rs
        l <- lateralD (rs ^. Pg.lateral)
        d <- selInline sel
        a <- subselectAlias rs sel
        pure (l <> d <> a)
    Just (Pg.Node'JoinExpr _) -> tableRefP n
    _ -> unsupported (nodeName n)


-- | "PgFmt.Render"'s @renderTableRefDoc@, for the right side of a join.
tableRefDoc :: Pg.Node -> R D
tableRefDoc n = case node n of
    Just (Pg.Node'RangeFunction rf) -> do
        wrap <- wrapEligibleFuncTable rf
        case wrap of
            Just f -> do
                l <- lateralD (rf ^. Pg.lateral)
                fn <- rangeFunctionNode rf
                e <- top fn
                d <- funcApplicationWrapped e f
                o <- ordinalityD rf
                after <- afterFunction rf
                a <- funcAliasClause MR rf after
                pure (l <> d <> o <> a)
            Nothing -> tableRefInline n
    Just (Pg.Node'RangeSubselect rs) -> do
        sel <- rangeSubselectSel rs
        l <- lateralD (rs ^. Pg.lateral)
        d <- selMaybeWrap sel
        a <- subselectAlias rs sel
        pure (l <> d <> a)
    _ -> tableRefInline n


-- | A table reference as postgresql-syntax writes it.
tableRefP :: Pg.Node -> R D
tableRefP n = case node n of
    Just (Pg.Node'RangeVar rv) -> relation rv
    Just (Pg.Node'RangeTableSample ts) -> tableSample ts
    Just (Pg.Node'RangeFunction rf) -> do
        l <- lateralD (rf ^. Pg.lateral)
        d <- funcTable MP rf
        after <- afterFunction rf
        a <- funcAliasClause MP rf after
        pure (l <> d <> a)
    Just (Pg.Node'RangeSubselect rs) -> do
        sel <- rangeSubselectSel rs
        l <- lateralD (rs ^. Pg.lateral)
        d <- selP sel
        a <- subselectAlias rs sel
        pure (l <> d <> a)
    Just (Pg.Node'JoinExpr j) -> do
        k <- joinParens j n
        joinP j k
    _ -> unsupported (nodeName n)


data JoinKind = CrossJoin | NaturalJoin | QualJoin


-- | A join's keywords as written (@left outer join@), and what kind of join
-- it is.
joinWords :: Pg.JoinExpr -> Pg.Node -> R ([Text], JoinKind)
joinWords j r = do
    when (isJust (j ^. Pg.maybe'alias) || isJust (j ^. Pg.maybe'joinUsingAlias)) (unsupported "a join with an alias")
    rFirst <- tableRefFirst r
    isJoin <- tokIs (rFirst - 1) "join"
    unless isJoin (unsupported "a join pgfmt cannot read")
    let collect i acc = do
            w <- tokL i
            if w `elem` ["inner", "left", "right", "full", "outer", "natural", "cross"]
                then collect (i - 1) (w : acc)
                else pure acc
    ws <- collect (rFirst - 2) []
    let kind
            | "natural" `elem` ws = NaturalJoin
            | "cross" `elem` ws = CrossJoin
            | otherwise = QualJoin
        joinType = filter (`notElem` ["natural", "cross"]) ws
        expected = case j ^. Pg.jointype of
            Pg.JOIN_INNER -> [[], ["inner"]]
            Pg.JOIN_LEFT -> [["left"], ["left", "outer"]]
            Pg.JOIN_RIGHT -> [["right"], ["right", "outer"]]
            Pg.JOIN_FULL -> [["full"], ["full", "outer"]]
            _ -> []
        hasQual = isJust (j ^. Pg.maybe'quals) || not (null (j ^. Pg.usingClause))
    unless (joinType `elem` expected) (unsupported "a join type pgfmt cannot read")
    case kind of
        NaturalJoin -> unless (j ^. Pg.isNatural && not hasQual) (unsupported "a natural join pgfmt cannot read")
        CrossJoin -> unless (not (j ^. Pg.isNatural) && not hasQual && null joinType) (unsupported "a cross join pgfmt cannot read")
        QualJoin -> unless (not (j ^. Pg.isNatural) && hasQual) (unsupported "a join without a condition")
    pure (ws ++ ["join"], kind)


-- | "PgFmt.Render"'s @renderJoinedTable@.
joinedTable :: Pg.JoinExpr -> Int -> R D
joinedTable j k
    | k > 0 = do
        inner <- joinedTable j (k - 1)
        pure ("(" <> inner <> ")")
    | otherwise = do
        l <- req "a join" (j ^. Pg.maybe'larg)
        r <- req "a join" (j ^. Pg.maybe'rarg)
        ld <- tableRef False l
        (ws, kind) <- joinWords j r
        keyword <- kw (T.unwords ws)
        rd <- tableRefDoc r
        let header = ld <> hardline <> (keyword <+> rd)
        case kind of
            QualJoin -> do
                q <- joinQual MR (T.length (fromMaybe "" (listToMaybe ws))) j r
                pure (header <> hardline <> q)
            _ -> pure header


joinQual :: Mode -> Int -> Pg.JoinExpr -> Pg.Node -> R D
joinQual mode anchor j r = case j ^. Pg.maybe'quals of
    Just q
        | mode == MR -> condAligned anchor "on" q
        | otherwise -> do
            k <- kw "on"
            d <- top q >>= expr MP
            pure (k <+> d)
    Nothing -> do
        rFirst <- tableRefFirst r
        let find i = do
                w <- tokL i
                t <- tokM i
                case t of
                    Nothing -> unsupported "a using pgfmt cannot find"
                    _ | w == "using" -> pure i
                    _ -> find (i + 1)
        u <- find rFirst
        _ <- expectParen (u + 1)
        k <- kw "using"
        cs <- strsOf (j ^. Pg.usingClause) >>= traverse ident
        pure (k <+> "(" <> commaJoin cs <> ")")


-- | A join as postgresql-syntax writes it, on one line.
joinP :: Pg.JoinExpr -> Int -> R D
joinP j k
    | k > 0 = do
        inner <- joinP j (k - 1)
        pure ("(" <> inner <> ")")
    | otherwise = do
        l <- req "a join" (j ^. Pg.maybe'larg)
        r <- req "a join" (j ^. Pg.maybe'rarg)
        ld <- tableRefP l
        (ws, kind) <- joinWords j r
        keyword <- kw (T.unwords ws)
        rd <- tableRefP r
        case kind of
            QualJoin -> do
                q <- joinQual MP 0 j r
                pure (ld <+> keyword <+> rd <+> q)
            _ -> pure (ld <+> keyword <+> rd)


----------------------------------------------------------------------
-- Statements.
----------------------------------------------------------------------

statement :: Pg.Node -> R D
statement n = case node n of
    Just (Pg.Node'SelectStmt s) -> do
        k <- selectParens 0 s
        selStmt s k
    Just (Pg.Node'InsertStmt s) -> insertStmt s
    Just (Pg.Node'UpdateStmt s) -> updateStmt s
    Just (Pg.Node'DeleteStmt s) -> deleteStmt s
    Just (Pg.Node'CreateStmt c) -> local (\env -> env{tightTypes = True}) (createTable c)
    Just (Pg.Node'ViewStmt v) -> createView v
    Just (Pg.Node'IndexStmt ix) -> createIndex ix
    Just (Pg.Node'AlterTableStmt a) -> local (\env -> env{tightTypes = True}) (alterTable a)
    Just (Pg.Node'CreateTableAsStmt c) -> createTableAs c
    Just (Pg.Node'CallStmt c) -> do
        f <- req "a call" (c ^. Pg.maybe'funccall)
        e <- top (defMessage `withFuncCall` f)
        k <- kw "call"
        d <- funcApplication MP e f
        pure (k <+> d)
    _ -> unsupported "not DML or DDL pgfmt lays out"


-- | "PgFmt.Render"'s @renderPreparableStmt@, for a common table
-- expression's body.
preparable :: Pg.Node -> R D
preparable n = case node n of
    Just (Pg.Node'SelectStmt s) -> do
        k <- selectParens 0 s
        selStmt s k
    Just (Pg.Node'InsertStmt s) -> insertStmt s
    Just (Pg.Node'UpdateStmt s) -> updateStmt s
    Just (Pg.Node'DeleteStmt s) -> deleteStmt s
    _ -> unsupported (nodeName n)


-- | Where a @returning@ clause starts.
returningAt :: Maybe Pg.ReturningClause -> R (Maybe Int)
returningAt = \case
    Just rc | t : _ <- rc ^. Pg.exprs -> do
        q <- targetFirst t
        maybe (pure Nothing) (wordsBefore ["returning"]) q
    _ -> pure Nothing


-- | A range variable as a node, for its locations.
toNode :: Pg.RangeVar -> Pg.Node
toNode rv = defMessage & Pg.rangeVar .~ rv


returning :: Maybe Pg.ReturningClause -> R (Maybe D)
returning Nothing = pure Nothing
returning (Just rc) = do
    unless (null (rc ^. Pg.options)) (unsupported "returning with old/new")
    k <- kw "returning"
    ds <- traverse targetEl (rc ^. Pg.exprs)
    pure (Just (k <+> commaJoin ds))


insertStmt :: Pg.InsertStmt -> R D
insertStmt s = do
    w <- traverse withClause (s ^. Pg.maybe'withClause)
    rv <- req "an insert" (s ^. Pg.maybe'relation)
    name <- relationName rv
    target <- case rv ^. Pg.maybe'alias of
        Nothing -> pure name
        Just a -> do
            asK <- kw "as"
            an <- ident (a ^. Pg.aliasname)
            pure (name <> " " <> asK <> " " <> an)
    ins <- kw "insert into"
    rest <- case s ^. Pg.maybe'selectStmt of
        Nothing -> kw "default values"
        Just q -> do
            cols <-
                if null (s ^. Pg.cols)
                    then pure Nothing
                    else Just <$> insertColumns (s ^. Pg.cols)
            override <- case s ^. Pg.override of
                Pg.OVERRIDING_USER_VALUE -> overriding "user"
                Pg.OVERRIDING_SYSTEM_VALUE -> overriding "system"
                _ -> pure mempty
            st <- case node q of
                Just (Pg.Node'SelectStmt st) -> pure st
                _ -> unsupported "an insert of something other than a select"
            k <- selectParens 0 st
            selD <- selStmt st k
            selAt <- attempt (subtract k <$> selectFirst st)
            case cols of
                Nothing -> pure (override <> selD)
                Just c -> do
                    b <- brkAt mempty selAt
                    pure (c <> b <> override <> selD)
    conflict <- traverse onConflict (s ^. Pg.maybe'onConflictClause)
    ret <- returning (s ^. Pg.maybe'returningClause)
    insAt <- clauseAt [["insert", "into"]] (toNode rv)
    restAt <- case s ^. Pg.maybe'selectStmt of
        Nothing -> pure Nothing
        Just q -> case (s ^. Pg.cols, node q) of
            (c : _, _) -> fmap (subtract 1) <$> targetFirst c
            (_, Just (Pg.Node'SelectStmt st)) -> attempt (selectParensPeek st >>= \k -> subtract k <$> selectFirst st)
            _ -> pure Nothing
    conflictAt <- case s ^. Pg.maybe'onConflictClause of
        Just oc | oc ^. Pg.location >= 0 -> Just <$> tokAt (fromIntegral (oc ^. Pg.location))
        _ -> pure Nothing
    retAt <- returningAt (s ^. Pg.maybe'returningClause)
    linesAt mempty (present [(Nothing, w), (insAt, Just (ins <+> target)), (restAt, Just rest), (conflictAt, conflict), (retAt, ret)])
  where
    overriding who = do
        k <- kw ("overriding " <> who <> " value")
        pure (k <> hardline)


-- | "PgFmt.Render"'s @renderInsertColumnList@.
insertColumns :: [Pg.Node] -> R D
insertColumns cols = do
    ds <- forM cols \n -> case node n of
        Just (Pg.Node'ResTarget rt) -> setTarget rt
        _ -> unsupported "an insert column pgfmt cannot read"
    first <- case cols of
        n : _ | Just (Pg.Node'ResTarget rt) <- node n -> tokAt (fromIntegral (rt ^. Pg.location))
        _ -> unsupported "an insert column list"
    _ <- expectParen (first - 1)
    close <- matchOf (first - 1) >>= req "an insert column list"
    commented <- commentsBetween (first - 1) close
    if commented
        then do
            firsts <- listFirsts (first - 1) close (length ds)
            indent indentSize <$> bracketListAt ((,close) <$> firsts) ds
        else
            pure
                if length ds <= 3
                    then indent indentSize ("(" <> commaJoin ds <> ")")
                    else indent indentSize (bracketList ds)


-- | A column being set: its name and indirection.
setTarget :: Pg.ResTarget -> R D
setTarget rt = do
    name <- ident (rt ^. Pg.name)
    i <- indirection MP defMessage (rt ^. Pg.indirection)
    pure (name <> i)


-- | An UPDATE or ON CONFLICT's assignments, a multi-column assignment
-- (@(a, b) = (1, 2)@) as one.
data SetItem
    = SetOne Pg.ResTarget Pg.Node
    | SetMany [Pg.ResTarget] Pg.Node


setItems :: [Pg.Node] -> R [SetItem]
setItems [] = pure []
setItems (n : rest) = case node n of
    Just (Pg.Node'ResTarget rt) -> case rt ^. Pg.maybe'val >>= node of
        Just (Pg.Node'MultiAssignRef m) -> do
            let count = fromIntegral (m ^. Pg.ncolumns) - 1
                (more, rest') = splitAt count rest
            others <- forM more \x -> case node x of
                Just (Pg.Node'ResTarget r) -> pure r
                _ -> unsupported "a multi-column assignment pgfmt cannot read"
            source <- req "a multi-column assignment" (m ^. Pg.maybe'source)
            (SetMany (rt : others) source :) <$> setItems rest'
        Just _ -> do
            v <- req "an assignment" (rt ^. Pg.maybe'val)
            (SetOne rt v :) <$> setItems rest
        Nothing -> unsupported "an assignment without a value"
    _ -> unsupported "an assignment pgfmt cannot read"


-- | An assignment's first token.
setFirst :: SetItem -> R (Maybe Int)
setFirst = \case
    SetOne rt _ -> at rt 0
    SetMany (rt : _) _ -> at rt 1
    SetMany [] _ -> pure Nothing
  where
    at rt back
        | rt ^. Pg.location >= 0 = Just . subtract back <$> tokAt (fromIntegral (rt ^. Pg.location))
        | otherwise = pure Nothing


-- | "PgFmt.Render"'s @renderSetClause@.
setClause :: SetItem -> R D
setClause = \case
    SetOne rt v -> do
        t <- setTarget rt
        d <- top v >>= exprMaybeWrap
        pure (t <+> "=" <+> nest indentSize d)
    SetMany rts source -> do
        first <- case rts of
            rt : _ -> tokAt (fromIntegral (rt ^. Pg.location))
            [] -> unsupported "an empty assignment"
        _ <- expectParen (first - 1)
        ts <- traverse setTarget rts
        d <- top source >>= exprMaybeWrap
        pure ("(" <> commaJoin ts <> ") =" <+> nest indentSize d)


onConflict :: Pg.OnConflictClause -> R D
onConflict oc = do
    header <- kw "on conflict"
    target <- case oc ^. Pg.maybe'infer of
        Nothing -> pure mempty
        Just inf
            | not (T.null (inf ^. Pg.conname)) -> do
                k <- kw "on constraint"
                n <- ident (inf ^. Pg.conname)
                pure (" " <> k <+> n)
            | otherwise -> do
                at <- tokAt (fromIntegral (oc ^. Pg.location))
                open <- nextParen at
                _ <- expectParen open
                elems <- forM (inf ^. Pg.indexElems) \n -> case node n of
                    Just (Pg.Node'IndexElem ie) -> do
                        unless (null (ie ^. Pg.collation) && null (ie ^. Pg.opclass) && ie ^. Pg.ordering == Pg.SORTBY_DEFAULT && ie ^. Pg.nullsOrdering == Pg.SORTBY_NULLS_DEFAULT) (unsupported "an index element with options")
                        case ie ^. Pg.maybe'expr of
                            Just x -> top x >>= expr MP
                            Nothing -> ident (ie ^. Pg.name)
                    _ -> unsupported "an index element pgfmt cannot read"
                w <- forM (inf ^. Pg.maybe'whereClause) \x -> do
                    k <- kw "where"
                    d <- top x >>= expr MP
                    pure (" " <> k <+> d)
                pure (" (" <> commaJoin elems <> ")" <> fromMaybe mempty w)
    action <- case oc ^. Pg.action of
        Pg.ONCONFLICT_NOTHING -> kw "do nothing"
        Pg.ONCONFLICT_UPDATE -> do
            items <- setItems (oc ^. Pg.targetList)
            k <- kw "do update set"
            docs <- traverse setClause items
            let lastIdx = length docs - 1
                body = case docs of
                    [single] -> k <> hardline <> indent indentSize single
                    _ -> k <> hardline <> indent indentSize (vsep [if i < lastIdx then d <> "," else d | (i, d) <- zip [0 ..] docs])
            w <- traverse (condAligned 5 "where") (oc ^. Pg.maybe'whereClause)
            pure (maybe body (\wd -> body <> hardline <> wd) w)
        _ -> unsupported "an on conflict pgfmt cannot read"
    pure (header <> target <> hardline <> action)


updateStmt :: Pg.UpdateStmt -> R D
updateStmt s = do
    w <- traverse withClause (s ^. Pg.maybe'withClause)
    rv <- req "an update" (s ^. Pg.maybe'relation)
    rel <- relation rv
    up <- kw "update"
    items <- setItems (s ^. Pg.targetList)
    setK <- kw "set"
    commented <- case items of
        [single] -> setFirst single >>= maybe (pure False) (fmap (not . null) . gapBefore)
        _ -> pure False
    sets <- case items of
        [single] | not commented -> (setK <+>) <$> setClause single
        _ -> blockList setK setClause setFirst items
    from <- if null (s ^. Pg.fromClause) then pure Nothing else Just <$> fromClause (s ^. Pg.fromClause)
    wh <- traverse whereOrCurrent (s ^. Pg.maybe'whereClause)
    ret <- returning (s ^. Pg.maybe'returningClause)
    upAt <- clauseAt [["update"]] (toNode rv)
    setAt <- case items of
        i : _ -> setFirst i >>= maybe (pure Nothing) (wordsBefore ["set"])
        [] -> pure Nothing
    fromAt <- maybe (pure Nothing) (clauseAt [["from"], ["from", "lateral"]]) (listToMaybe (s ^. Pg.fromClause))
    whAt <- maybe (pure Nothing) (clauseAt [["where"], ["where", "current", "of"]]) (s ^. Pg.maybe'whereClause)
    retAt <- returningAt (s ^. Pg.maybe'returningClause)
    linesAt mempty (present [(Nothing, w), (upAt, Just (up <+> rel)), (setAt, Just sets), (fromAt, from), (whAt, wh), (retAt, ret)])


deleteStmt :: Pg.DeleteStmt -> R D
deleteStmt s = do
    w <- traverse withClause (s ^. Pg.maybe'withClause)
    rv <- req "a delete" (s ^. Pg.maybe'relation)
    rel <- relation rv
    del <- kw "delete from"
    using <-
        if null (s ^. Pg.usingClause)
            then pure Nothing
            else do
                k <- kw "using"
                ds <- traverse tableRefP (s ^. Pg.usingClause)
                pure (Just (k <+> commaJoin ds))
    wh <- traverse whereOrCurrent (s ^. Pg.maybe'whereClause)
    ret <- returning (s ^. Pg.maybe'returningClause)
    delAt <- clauseAt [["delete", "from"]] (toNode rv)
    usingAt <- maybe (pure Nothing) (clauseAt [["using"]]) (listToMaybe (s ^. Pg.usingClause))
    whAt <- maybe (pure Nothing) (clauseAt [["where"], ["where", "current", "of"]]) (s ^. Pg.maybe'whereClause)
    retAt <- returningAt (s ^. Pg.maybe'returningClause)
    linesAt mempty (present [(Nothing, w), (delAt, Just (del <+> rel)), (usingAt, using), (whAt, wh), (retAt, ret)])


whereOrCurrent :: Pg.Node -> R D
whereOrCurrent n = case node n of
    Just (Pg.Node'CurrentOfExpr c) -> do
        k <- kw "where current of"
        name <- ident (c ^. Pg.cursorName)
        pure (k <+> name)
    _ -> condAligned 5 "where" n


----------------------------------------------------------------------
-- DDL: create table, create view, create table / materialized view as.
--
-- The keywords of a clause are the source's own words (@temp@ or
-- @temporary@, an explicit @on delete no action@), in the keyword case;
-- names, types and expressions come from the tree, as in DML. Anything
-- else (storage options, @exclude@, @partition of@, ...) is not laid out.
----------------------------------------------------------------------

-- | The statement's last token, before any semicolon.
lastTok :: R Int
lastTok = do
    ts <- asks (.toks)
    case IM.lookupMax ts of
        Just (i, t) | t.text == ";" -> pure (i - 1)
        Just (i, _) -> pure i
        Nothing -> unsupported "an empty statement"


data Piece = Word | Open | Glue | Close


-- | The tokens from @a@ to before @b@, which must be keywords, numbers, and
-- parentheses and commas between them, as words in the keyword case:
-- @not null@, @on delete set null@, @identity (start with 1 cache 10)@.
wordsRun :: Int -> Int -> R D
wordsRun a b = do
    pieces <- forM [a .. b - 1] \j -> do
        t <- tokM j
        case t of
            Just tk
                | tk.text == "(" -> do
                    _ <- expectParen j
                    pure (Open, "(")
                | tk.text `elem` [")", ","] -> pure (Close, pretty tk.text)
                | tk.text == "-" -> pure (Glue, "-")
                | tk.kind `elem` [Pg.ICONST, Pg.FCONST] -> pure (Word, pretty (T.toLower tk.text))
                | tk.isKeyword && tk.kind /= Pg.IDENT -> (Word,) <$> kw (T.toLower tk.text)
            _ -> unsupported "a clause pgfmt cannot read"
    pure (joined pieces)
  where
    joined = \case
        [] -> mempty
        (k, d) : rest -> d <> go k rest
    go _ [] = mempty
    go prev ((k, d) : rest) = sep prev k <> d <> go k rest
    -- No space after an opening parenthesis or a minus, or before a
    -- closing parenthesis or a comma.
    sep Open _ = mempty
    sep Glue _ = mempty
    sep _ Close = mempty
    sep _ _ = " "


-- | 'wordsRun', with a space before it, if there are any tokens.
tailRun :: Int -> Int -> R D
tailRun a b
    | a >= b = pure mempty
    | otherwise = (" " <>) <$> wordsRun a b


-- | A parenthesized list of names, @(a, b)@, at this token: the list and
-- the token after it.
nameList :: Int -> [Text] -> R (D, Int)
nameList open names = do
    close <- expectParen open
    unless (not (null names) && close == open + 2 * length names) (unsupported "a list of names pgfmt cannot read")
    ds <- zipWithM (\k n -> identAt (open + 1 + 2 * k) n) [0 ..] names
    pure ("(" <> commaJoin ds <> ")", close + 1)


-- | How many tokens a relation's dotted name has, less its dots.
nameParts :: Pg.RangeVar -> Int
nameParts rv = length (filter (not . T.null) [rv ^. Pg.catalogname, rv ^. Pg.schemaname, rv ^. Pg.relname])


-- | A relation's name and the token after it.
relationAt :: Pg.RangeVar -> R (D, Int, Int)
relationAt rv = do
    at <- tokAt (fromIntegral (rv ^. Pg.location))
    name <- relationName rv
    pure (name, at, at + 2 * nameParts rv - 1)


-- | @create table@: one column or constraint per line, each but the last
-- followed by its comma.
createTable :: Pg.CreateStmt -> R D
createTable c = do
    rv <- req "a create table" (c ^. Pg.maybe'relation)
    when
        (isJust (c ^. Pg.maybe'partbound) || isJust (c ^. Pg.maybe'ofTypename) || not (null (c ^. Pg.inhRelations)))
        (unsupported "a create table pgfmt does not lay out")
    (name, relAt, open) <- relationAt rv
    header <- wordsRun 0 relAt
    close <- expectParen open
    let elts = c ^. Pg.tableElts
    starts <- traverse elementStart elts
    case starts of
        [] -> unless (close == open + 1) (unsupported "a create table pgfmt cannot read")
        s0 : rest -> do
            unless (s0 == open + 1) (unsupported "a create table pgfmt cannot read")
            forM_ rest \s -> do
                comma <- tokIs (s - 1) ","
                unless comma (unsupported "a create table pgfmt cannot read")
    let ends = map (subtract 1) (drop 1 starts) ++ [close]
        n = length elts
    lines' <- forM (zip4 [0 :: Int ..] elts starts ends) \(i, el, s, e) -> do
        d <- tableElement el s e
        b <- brk mempty s
        -- A blank line between two elements is kept, as between values rows.
        blank <- asks (IS.member s . (.blankLineBefore))
        let gap = if blank && i > 0 then hardline else mempty
        pure (b <> gap <> d <> (if i < n - 1 then "," else mempty))
    -- Comments before the closing parenthesis: one sharing the last line
    -- ends it, one on a line of its own stays one, indented with the rows.
    cs <- gapBefore close
    let (trailing, own) = span (not . (.ownLine)) cs
        endComments =
            mconcat [" " <> raw x.ctext | x <- trailing]
                <> mconcat [hardline <> (if x.blankBefore then hardline else mempty) <> raw x.ctext | x <- own]
        body
            | n == 0 && null cs = "()"
            | otherwise = "(" <> nest indentSize (mconcat lines' <> endComments) <> hardline <> ")"
    tl <- tableTail c (close + 1)
    pure (header <+> name <+> body <> tl)


-- | Where a table element starts.
elementStart :: Pg.Node -> R Int
elementStart el = case node el of
    Just (Pg.Node'ColumnDef cd) -> tokAt (fromIntegral (cd ^. Pg.location))
    Just (Pg.Node'Constraint k) -> tokAt (fromIntegral (k ^. Pg.location))
    Just (Pg.Node'TableLikeClause l) -> do
        rv <- req "a like clause" (l ^. Pg.maybe'relation)
        at <- tokAt (fromIntegral (rv ^. Pg.location))
        isLike <- tokIs (at - 1) "like"
        unless isLike (unsupported "a like clause pgfmt cannot read")
        pure (at - 1)
    _ -> unsupported "a table element pgfmt cannot read"


-- | A column, constraint or @like@ clause, from token @s@ to before @e@.
tableElement :: Pg.Node -> Int -> Int -> R D
tableElement el s e = case node el of
    Just (Pg.Node'ColumnDef cd) -> columnDef cd s e
    Just (Pg.Node'Constraint k) -> constraint k s e
    Just (Pg.Node'TableLikeClause l) -> do
        rv <- req "a like clause" (l ^. Pg.maybe'relation)
        k <- kw "like"
        (name, _, after) <- relationAt rv
        opts <- tailRun after e
        pure (k <+> name <> opts)
    _ -> unsupported "a table element pgfmt cannot read"


columnDef :: Pg.ColumnDef -> Int -> Int -> R D
columnDef cd s e = do
    when
        ( not (T.null (cd ^. Pg.compression))
            || not (T.null (cd ^. Pg.storageName))
            || isJust (cd ^. Pg.maybe'collClause)
            || not (null (cd ^. Pg.fdwoptions))
            || isJust (cd ^. Pg.maybe'rawDefault)
        )
        (unsupported "a column pgfmt does not lay out")
    name <- identAt s (cd ^. Pg.colname)
    tn <- req "a column" (cd ^. Pg.maybe'typeName)
    tyAt <- tokAt (fromIntegral (tn ^. Pg.location))
    unless (tyAt == s + 1) (unsupported "a column pgfmt cannot read")
    ty <- typeName tn
    ks <- forM (cd ^. Pg.constraints) \k -> case node k of
        Just (Pg.Node'Constraint x) -> (x,) <$> tokAt (fromIntegral (x ^. Pg.location))
        _ -> unsupported "a column constraint pgfmt cannot read"
    let kends = map snd (drop 1 ks) ++ [e]
    kds <- forM (zip ks kends) \((x, a), b) -> constraint x a b
    pure (spaceJoin (name : ty : kds))


-- | A constraint, from token @a@ to before @b@, with its name if it has one.
constraint :: Pg.Constraint -> Int -> Int -> R D
constraint x a b
    | T.null (x ^. Pg.conname) = constraintBody 0 x a b
    | otherwise = do
        isCon <- tokIs a "constraint"
        unless isCon (unsupported "a constraint pgfmt cannot read")
        k <- kw "constraint"
        n <- identAt (a + 1) (x ^. Pg.conname)
        -- Comments between the name and the body: the body goes on the
        -- line after them, indented.
        cs <- gapBefore (a + 2)
        d <- constraintBody (if null cs then len (k <+> n) + 1 else 0) x (a + 2) b
        if null cs
            then pure (k <+> n <+> d)
            else do
                br <- brk mempty (a + 2)
                pure (k <+> n <> nest indentSize (br <> d))


-- | A constraint without its name, which takes @before@ characters of its
-- line.
constraintBody :: Int -> Pg.Constraint -> Int -> Int -> R D
constraintBody before x a b = case x ^. Pg.contype of
    Pg.CONSTR_NULL -> wordsRun a b
    Pg.CONSTR_NOTNULL | null (x ^. Pg.keys) -> wordsRun a b
    Pg.CONSTR_IDENTITY -> wordsRun a b
    k | k `elem` [Pg.CONSTR_ATTR_DEFERRABLE, Pg.CONSTR_ATTR_NOT_DEFERRABLE, Pg.CONSTR_ATTR_DEFERRED, Pg.CONSTR_ATTR_IMMEDIATE] -> wordsRun a b
    Pg.CONSTR_DEFAULT -> do
        isDefault <- tokIs a "default"
        unless isDefault (unsupported "a default pgfmt cannot read")
        k <- kw "default"
        ex <- req "a default" (x ^. Pg.maybe'rawExpr)
        d <- ddlExpr ex
        pure (k <+> d)
    Pg.CONSTR_GENERATED -> do
        open <- parenWithin
        w <- wordsRun a open
        close <- expectParen open
        ex <- req "a generated column" (x ^. Pg.maybe'rawExpr)
        d <- ddlExpr ex
        t <- tailRun (close + 1) b
        pure (w <+> "(" <> d <> ")" <> t)
    Pg.CONSTR_CHECK -> do
        open <- parenWithin
        w <- wordsRun a open
        close <- expectParen open
        ex <- req "a check" (x ^. Pg.maybe'rawExpr)
        d <- checkCondition ex
        t <- tailRun (close + 1) b
        pure (w <+> d <> t)
    k | k `elem` [Pg.CONSTR_PRIMARY, Pg.CONSTR_UNIQUE] -> do
        when
            ( not (null (x ^. Pg.including))
                || not (null (x ^. Pg.options))
                || not (T.null (x ^. Pg.indexname))
                || not (T.null (x ^. Pg.indexspace))
                || isJust (x ^. Pg.maybe'whereClause)
                || x ^. Pg.withoutOverlaps
            )
            (unsupported "a key pgfmt does not lay out")
        if null (x ^. Pg.keys)
            then wordsRun a b
            else do
                open <- parenWithin
                w <- wordsRun a open
                names <- strsOf (x ^. Pg.keys)
                (l, after) <- nameList open names
                t <- tailRun after b
                pure (w <+> l <> t)
    Pg.CONSTR_FOREIGN -> do
        when
            (not (null (x ^. Pg.fkDelSetCols)) || x ^. Pg.fkWithPeriod || x ^. Pg.pkWithPeriod)
            (unsupported "a foreign key pgfmt does not lay out")
        (lead, refAt) <-
            if null (x ^. Pg.fkAttrs)
                then pure (mempty, a)
                else do
                    open <- parenWithin
                    w <- wordsRun a open
                    names <- strsOf (x ^. Pg.fkAttrs)
                    (l, after) <- nameList open names
                    pure (w <+> l, after)
        isRef <- tokIs refAt "references"
        unless isRef (unsupported "a foreign key pgfmt cannot read")
        k <- kw "references"
        pk <- req "a foreign key" (x ^. Pg.maybe'pktable)
        (name, pkAt, after) <- relationAt pk
        unless (pkAt == refAt + 1) (unsupported "a foreign key pgfmt cannot read")
        (cols, rest) <-
            if null (x ^. Pg.pkAttrs)
                then pure (mempty, after)
                else strsOf (x ^. Pg.pkAttrs) >>= nameList after
        t <- tailRun rest b
        let refs = k <+> name <> cols <> t
        -- A long one breaks before @references@.
        pure
            if
                | null (x ^. Pg.fkAttrs) -> refs
                | before + len (lead <+> refs) <= foreignKeyInlineThreshold -> lead <+> refs
                | otherwise -> nest indentSize (lead <> hardline <> refs)
    _ -> unsupported "a constraint pgfmt does not lay out"
  where
    parenWithin = do
        open <- nextParen a
        unless (open < b) (unsupported "a constraint pgfmt cannot read")
        pure open


-- | A check constraint's condition, in its parentheses: on one line if
-- short, otherwise broken as 'boolBlock' breaks it.
checkCondition :: Pg.Node -> R D
checkCondition n = do
    e <- top n
    inline <- expr MR e
    if e.lv == 0 && isAndOr n && len inline > checkInlineThreshold
        then do
            b <- boolBlock e
            pure ("(" <> nest indentSize (hardline <> b) <> hardline <> ")")
        else do
            d <- ddlExpr n
            pure ("(" <> d <> ")")


-- | Whether a node is an @and@ or @or@.
isAndOr :: Pg.Node -> Bool
isAndOr n = case node n of
    Just (Pg.Node'BoolExpr be) -> be ^. Pg.boolop `elem` [Pg.AND_EXPR, Pg.OR_EXPR]
    _ -> False


-- | A long condition in DDL: each operand of its top-level @and@ (or @or@)
-- on a line of its own, the operator first; an operand in parentheses that
-- is long itself broken the same way inside them.
boolBlock :: E -> R D
boolBlock e = case node e.en of
    Just (Pg.Node'BoolExpr be)
        | Just op <- lookup (be ^. Pg.boolop) [(Pg.AND_EXPR, "and"), (Pg.OR_EXPR, "or")]
        , first : rest <- be ^. Pg.args -> do
            opK <- kw op
            fd <- child e.en first >>= boolPart
            rds <- traverse (child e.en >=> boolPart) rest
            pure (vsep (fd : map (opK <+>) rds))
    _ -> expr MR e


-- | An operand of 'boolBlock'.
boolPart :: E -> R D
boolPart c = do
    inline <- expr MR c
    if c.lv == 1 && isAndOr c.en && len inline > checkInlineThreshold
        then do
            inner <- boolBlock c{lv = 0}
            pure ("(" <> nest indentSize (hardline <> inner) <> hardline <> ")")
        else pure inline


-- | An index's @where@: on one line if short, otherwise broken as
-- 'boolBlock' breaks it.
indexWhere :: Pg.Node -> R D
indexWhere n = do
    k <- kw "where"
    e <- top n
    inline <- expr MR e
    if
        | len inline <= checkInlineThreshold || not (isAndOr n) -> pure (k <+> inline)
        | e.lv == 0 -> do
            b <- boolBlock e
            pure (k <> nest indentSize (hardline <> b))
        | otherwise -> (k <+>) <$> boolPart e


-- | An expression in a column or constraint: on one line if short,
-- otherwise wrapped as a long select target is.
ddlExpr :: Pg.Node -> R D
ddlExpr n = do
    e <- top n
    inline <- expr MR e
    eligible <- isWrapEligible e
    if len inline > checkInlineThreshold && eligible
        then exprWrapped e
        else pure inline


-- | The longest table foreign key, with its name, that stays on one line.
foreignKeyInlineThreshold :: Int
foreignKeyInlineThreshold = 90


-- | The longest check condition that stays on one line.
checkInlineThreshold :: Int
checkInlineThreshold = 80


-- | What follows a table's columns: @partition by range (a)@, @on commit
-- drop@.
tableTail :: Pg.CreateStmt -> Int -> R D
tableTail c from = do
    end <- (+ 1) <$> lastTok
    case c ^. Pg.maybe'partspec of
        Nothing -> tailRun from end
        Just ps -> do
            pAt <- tokAt (fromIntegral (ps ^. Pg.location))
            pre <- tailRun from pAt
            open <- nextParen pAt
            w <- wordsRun pAt open
            names <- forM (ps ^. Pg.partParams) \p -> case node p of
                Just (Pg.Node'PartitionElem pe)
                    | not (T.null (pe ^. Pg.name))
                    , isNothing (pe ^. Pg.maybe'expr)
                    , null (pe ^. Pg.collation)
                    , null (pe ^. Pg.opclass) ->
                        pure (pe ^. Pg.name)
                _ -> unsupported "a partition key pgfmt does not lay out"
            (l, after) <- nameList open names
            post <- tailRun after end
            pure (pre <> " " <> w <+> l <> post)


-- | @create view@: its query laid out as a statement of its own, under
-- the line that names the view.
createView :: Pg.ViewStmt -> R D
createView v = do
    rv <- req "a view" (v ^. Pg.maybe'view)
    unless (null (v ^. Pg.options)) (unsupported "a view with options")
    aliases <- strsOf (v ^. Pg.aliases)
    q <- req "a view" (v ^. Pg.maybe'query)
    checkOption <- case v ^. Pg.withCheckOption of
        o | o `elem` [Pg.LOCAL_CHECK_OPTION, Pg.CASCADED_CHECK_OPTION] -> do
            end <- lastTok
            ws <- traverse tokL [end - 3 .. end]
            from <- case ws of
                ["with", m, "check", "option"] | m `elem` ["local", "cascaded"] -> pure (end - 3)
                [_, "with", "check", "option"] -> pure (end - 2)
                _ -> unsupported "a check option pgfmt cannot read"
            Just <$> wordsRun from (end + 1)
        _ -> pure Nothing
    asQuery rv aliases q checkOption


-- | @create table ... as@ and @create materialized view ... as@.
createTableAs :: Pg.CreateTableAsStmt -> R D
createTableAs c = do
    unless (c ^. Pg.objtype `elem` [Pg.OBJECT_TABLE, Pg.OBJECT_MATVIEW]) (unsupported "a create as pgfmt does not lay out")
    when (c ^. Pg.isSelectInto) (unsupported "select into")
    into <- req "a create as" (c ^. Pg.maybe'into)
    when
        ( not (null (into ^. Pg.options))
            || not (T.null (into ^. Pg.accessMethod))
            || not (T.null (into ^. Pg.tableSpaceName))
        )
        (unsupported "a create as pgfmt does not lay out")
    rv <- req "a create as" (into ^. Pg.maybe'rel)
    cols <- strsOf (into ^. Pg.colNames)
    q <- req "a create as" (c ^. Pg.maybe'query)
    end <- lastTok
    ws <- traverse tokL [end - 2 .. end]
    withData <- case ws of
        ["with", "no", "data"] -> Just <$> wordsRun (end - 2) (end + 1)
        [_, "with", "data"] -> Just <$> wordsRun (end - 1) (end + 1)
        _ -> pure Nothing
    asQuery rv cols q withData


-- | The head of a @create ... as@ (its words, the name, the column names),
-- its query on the lines after it, and what follows the query on a line of
-- its own.
asQuery :: Pg.RangeVar -> [Text] -> Pg.Node -> Maybe D -> R D
asQuery rv cols q after = do
    (name, relAt, next) <- relationAt rv
    recursive <- anyM (`tokIs` "recursive") [0 .. relAt - 1]
    when recursive (unsupported "a recursive view")
    header <- wordsRun 0 relAt
    (colsD, asAt) <-
        if null cols
            then pure (mempty, next)
            else do
                (l, nx) <- nameList next cols
                pure (" " <> l, nx)
    -- Words before @as@: @on commit drop@.
    asAt' <- findAs asAt
    between <- tailRun asAt asAt'
    asK <- kw "as"
    s <- case node q of
        Just (Pg.Node'SelectStmt s) -> pure s
        _ -> unsupported "a query pgfmt does not lay out"
    k <- selectParens 0 s
    body <- selStmt s k
    first <- selectFirst s
    b <- brk mempty first
    pure (header <+> name <> colsD <> between <+> asK <> b <> body <> maybe mempty (hardline <>) after)
  where
    findAs j = do
        t <- tokM j
        case t of
            Just tk | T.toLower tk.text == "as" -> pure j
            Just tk | tk.isKeyword && tk.kind /= Pg.IDENT -> findAs (j + 1)
            _ -> unsupported "a create as pgfmt cannot read"


-- | The top-level commas from token @a@ to before @b@: where each of the
-- parts they separate starts.
partsBetween :: Int -> Int -> R [Int]
partsBetween a b = go a [a]
  where
    go j acc
        | j >= b = pure (reverse acc)
        | otherwise = do
            t <- tokL j
            case t of
                "(" -> do
                    m <- matchOf j
                    maybe (unsupported "an unclosed parenthesis") (\c -> go (c + 1) acc) m
                "," -> go (j + 1) (j + 1 : acc)
                _ -> go (j + 1) acc


-- | @create index@: the index's name on its first line, then @on@ the
-- table and its columns, and the @where@ condition, each on a line of its
-- own.
createIndex :: Pg.IndexStmt -> R D
createIndex ix = do
    when
        ( not (null (ix ^. Pg.indexIncludingParams))
            || not (null (ix ^. Pg.options))
            || not (T.null (ix ^. Pg.tableSpace))
            || not (null (ix ^. Pg.excludeOpNames))
        )
        (unsupported "an index pgfmt does not lay out")
    rv <- req "an index" (ix ^. Pg.maybe'relation)
    (rel, relAt, afterRel) <- relationAt rv
    isOn <- tokIs (relAt - 1) "on"
    unless isOn (unsupported "an index pgfmt cannot read")
    let named = not (T.null (ix ^. Pg.idxname))
        nameAt = relAt - 2
    header <- wordsRun 0 (if named then nameAt else relAt - 1)
    nameD <- if named then (" " <>) <$> identAt nameAt (ix ^. Pg.idxname) else pure mempty
    onK <- kw "on"
    -- @using gin@, if written.
    usingWritten <- tokIs afterRel "using"
    (usingD, open) <-
        if usingWritten
            then do
                u <- kw "using"
                m <- identAt (afterRel + 1) (ix ^. Pg.accessMethod)
                pure (" " <> u <+> m, afterRel + 2)
            else pure (mempty, afterRel)
    close <- expectParen open
    starts <- partsBetween (open + 1) close
    unless (length starts == length (ix ^. Pg.indexParams)) (unsupported "an index pgfmt cannot read")
    let ends = map (subtract 1) (drop 1 starts) ++ [close]
    cols <- forM (zip3 (ix ^. Pg.indexParams) starts ends) \(p, s, e) -> case node p of
        Just (Pg.Node'IndexElem el) -> indexElem el s e
        _ -> unsupported "an index column pgfmt cannot read"
    whereAt <- maybe (pure Nothing) (clauseAt [["where"]]) (ix ^. Pg.maybe'whereClause)
    end <- (+ 1) <$> lastTok
    between <- tailRun (close + 1) (fromMaybe end whereAt)
    wh <- traverse indexWhere (ix ^. Pg.maybe'whereClause)
    let onLine = onK <+> rel <> usingD <+> "(" <> commaJoin cols <> ")" <> between
    onBreak <- brk mempty (relAt - 1)
    whBreak <- brkAt mempty whereAt
    pure (header <> nameD <> nest indentSize (onBreak <> onLine <> maybe mempty (whBreak <>) wh))


-- | An index column, from token @s@ to before @e@: a column or an
-- expression, its operator class, its order.
indexElem :: Pg.IndexElem -> Int -> Int -> R D
indexElem el s e = do
    unless (null (el ^. Pg.collation) && null (el ^. Pg.opclassopts) && T.null (el ^. Pg.indexcolname)) (unsupported "an index column pgfmt does not lay out")
    (d, next) <- case el ^. Pg.maybe'expr of
        Nothing -> do
            n <- identAt s (el ^. Pg.name)
            pure (n, s + 1)
        Just ex -> do
            -- An expression in parentheses, or a function call: it ends at
            -- the first parenthesis's match.
            open <- nextParen s
            close <- matchOf open >>= req "an index expression"
            d <- top ex >>= expr MR
            pure (d, close + 1)
    opclass <- strsOf (el ^. Pg.opclass)
    (opD, after) <-
        if null opclass
            then pure (mempty, next)
            else do
                q <- qualNameAt next opclass
                pure (" " <> q, next + 2 * length opclass - 1)
    t <- tailRun after e
    pure (d <> opD <> t)


-- | @alter table@: each action on a line of its own, each but the last
-- followed by its comma.
alterTable :: Pg.AlterTableStmt -> R D
alterTable a = do
    unless (a ^. Pg.objtype == Pg.OBJECT_TABLE) (unsupported "an alter pgfmt does not lay out")
    rv <- req "an alter table" (a ^. Pg.maybe'relation)
    (rel, relAt, afterRel) <- relationAt rv
    header <- wordsRun 0 relAt
    end <- (+ 1) <$> lastTok
    starts <- partsBetween afterRel end
    let cmds = a ^. Pg.cmds
        ends = map (subtract 1) (drop 1 starts) ++ [end]
        n = length cmds
    unless (length starts == n) (unsupported "an alter table pgfmt cannot read")
    lines' <- forM (zip4 [0 :: Int ..] cmds starts ends) \(i, c, s, e) -> do
        cmd <- case node c of
            Just (Pg.Node'AlterTableCmd x) -> pure x
            _ -> unsupported "an alter table pgfmt cannot read"
        d <- alterCmd cmd s e
        b <- brk mempty s
        blank <- asks (IS.member s . (.blankLineBefore))
        let gap = if blank && i > 0 then hardline else mempty
        pure (b <> gap <> d <> (if i < n - 1 then "," else mempty))
    pure (header <+> rel <> nest indentSize (mconcat lines'))


-- | One action of an @alter table@, from token @s@ to before @e@.
alterCmd :: Pg.AlterTableCmd -> Int -> Int -> R D
alterCmd c s e = case c ^. Pg.subtype of
    Pg.AT_AddColumn -> case def of
        Just (Pg.Node'ColumnDef cd) -> do
            at <- tokAt (fromIntegral (cd ^. Pg.location))
            w <- wordsRun s at
            d <- columnDef cd at e
            pure (w <+> d)
        _ -> cannot
    Pg.AT_AddConstraint -> case def of
        Just (Pg.Node'Constraint x) -> do
            at <- tokAt (fromIntegral (x ^. Pg.location))
            w <- wordsRun s at
            d <- constraint x at e
            pure (w <+> d)
        _ -> cannot
    t | t `elem` [Pg.AT_DropColumn, Pg.AT_DropConstraint, Pg.AT_ValidateConstraint, Pg.AT_DropNotNull, Pg.AT_SetNotNull, Pg.AT_DropIdentity, Pg.AT_EnableTrig, Pg.AT_DisableTrig, Pg.AT_EnableAlwaysTrig, Pg.AT_EnableReplicaTrig] -> withName Nothing
    Pg.AT_ColumnDefault -> withName (c ^. Pg.maybe'def)
    t | t `elem` [Pg.AT_EnableRowSecurity, Pg.AT_DisableRowSecurity, Pg.AT_ForceRowSecurity, Pg.AT_NoForceRowSecurity, Pg.AT_SetLogged, Pg.AT_SetUnLogged, Pg.AT_EnableTrigAll, Pg.AT_DisableTrigAll, Pg.AT_EnableTrigUser, Pg.AT_DisableTrigUser] -> wordsRun s e
    Pg.AT_AlterColumnType -> case def of
        Just (Pg.Node'ColumnDef cd) | isNothing (cd ^. Pg.maybe'collClause) -> do
            nameAt <- nameToken
            w <- wordsRun s nameAt
            nm <- identAt nameAt (c ^. Pg.name)
            tn <- req "an alter column" (cd ^. Pg.maybe'typeName)
            tyAt <- tokAt (fromIntegral (tn ^. Pg.location))
            w2 <- wordsRun (nameAt + 1) tyAt
            ty <- typeName tn
            using <- case cd ^. Pg.maybe'rawDefault of
                Nothing -> pure mempty
                Just ex -> do
                    u <- kw "using"
                    d <- ddlExpr ex
                    pure (" " <> u <+> d)
            pure (w <+> nm <+> w2 <+> ty <> using)
        _ -> cannot
    t | t `elem` [Pg.AT_SetRelOptions, Pg.AT_ResetRelOptions] -> do
        open <- nextParen s
        w <- wordsRun s open
        close <- expectParen open
        opts <- case def of
            Just (Pg.Node'List l) -> forM (l ^. Pg.items) \o -> case node o of
                Just (Pg.Node'DefElem de) | T.null (de ^. Pg.defnamespace) -> do
                    at <- tokAt (fromIntegral (de ^. Pg.location))
                    nm <- identAt at (de ^. Pg.defname)
                    case de ^. Pg.maybe'arg of
                        Nothing -> pure nm
                        Just _ -> do
                            eq <- tokIs (at + 1) "="
                            v <- tokM (at + 2)
                            case v of
                                Just vt | eq, vt.kind `elem` [Pg.ICONST, Pg.FCONST] -> pure (nm <> " = " <> pretty (T.toLower vt.text))
                                Just vt | eq, vt.kind == Pg.SCONST -> pure (nm <> " = " <> raw vt.text)
                                _ -> cannot
                _ -> cannot
            _ -> cannot
        t' <- tailRun (close + 1) e
        pure (w <+> "(" <> commaJoin opts <> ")" <> t')
    _ -> unsupported "an alter table action pgfmt does not lay out"
  where
    def = c ^. Pg.maybe'def >>= node
    cannot :: R a
    cannot = unsupported "an alter table action pgfmt cannot read"
    -- The named column, constraint or trigger: after the keywords that
    -- start the action.
    nameToken = go (s + 1)
      where
        go j = do
            w <- tokL j
            if w `elem` ["column", "constraint", "trigger", "if", "exists"] then go (j + 1) else pure j
    withName mexpr = do
        nameAt <- nameToken
        unless (nameAt < e) cannot
        w <- wordsRun s nameAt
        nm <- identAt nameAt (c ^. Pg.name)
        case mexpr of
            Nothing -> do
                t <- tailRun (nameAt + 1) e
                pure (w <+> nm <> t)
            Just ex -> do
                -- @set default@, then the expression.
                defaultAt <- findDefault (nameAt + 1)
                w2 <- wordsRun (nameAt + 1) (defaultAt + 1)
                d <- ddlExpr ex
                pure (w <+> nm <+> w2 <+> d)
    findDefault j
        | j >= e = cannot
        | otherwise = do
            w <- tokL j
            if w == "default" then pure j else findDefault (j + 1)
