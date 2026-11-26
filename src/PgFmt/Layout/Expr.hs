-- | Expressions, inline and wrapped, function calls, subqueries and
-- conditions.
module PgFmt.Layout.Expr where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (local)
import Control.Monad.State.Strict (gets, modify')
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.IntSet qualified as IS
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import PgFmt.Layout.Comments
import {-# SOURCE #-} PgFmt.Layout.Dml (selMaybeWrap, selP, selWrapped, selectParens, sortClause)
import PgFmt.Layout.Locations
import PgFmt.Layout.Monad
import PgFmt.Layout.Names
import PgFmt.Layout.Parens
import PgQuery qualified as Pg
import Prettyprinter (align, hardline, hcat, nest, pretty, punctuate, vsep, (<+>))


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


(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g


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
                    Just (Pg.Node'SubLink s)
                        | s ^. Pg.subLinkType == Pg.ANY_SUBLINK
                        , null (s ^. Pg.operName) -> do
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
        mine <- gets (IS.member p . (.boolGroups))
        mm <- matchOf p
        case mm of
            Just m | not placed || mine -> do
                let candidates = [(take k (x : rest), drop k (x : rest)) | k <- [length rest + 1, length rest .. 2]]
                found <- firstM (encloses p m . fst) candidates
                case found of
                    Just (inside, after) -> do
                        consume p
                        modify' \st -> st{boolGroups = IS.insert p st.boolGroups}
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
    | -- | @trim(both 'x' from y)@.
      Trim
        { modifier :: Maybe Text
        -- ^ @both@, @leading@ or @trailing@.
        , chars :: Maybe Pg.Node
        -- ^ The characters to trim, before @from@.
        , hasFrom :: Bool
        , strings :: [Pg.Node]
        }
    | -- | @overlay(x placing y from a for b)@.
      Overlay {string :: Pg.Node, placing :: Pg.Node, from :: Pg.Node, for :: Maybe Pg.Node}
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
                ([_, fn], _)
                    | fn `elem` ["btrim", "ltrim", "rtrim"]
                    , sqlSyntax -> do
                        let modifier = case [w | (_, w) <- pieces, w `elem` ["both", "leading", "trailing"]] of
                                w : _ -> Just w
                                [] -> Nothing
                            hasFrom = "from" `elem` map snd pieces
                        case args of
                            [s] -> pure Trim{modifier, chars = Nothing, hasFrom, strings = [s]}
                            [s, chars]
                                | hasFrom -> pure Trim{modifier, chars = Just chars, hasFrom = True, strings = [s]}
                                | otherwise -> pure Trim{modifier, chars = Nothing, hasFrom = False, strings = [s, chars]}
                            _ -> unsupported "a trim pgfmt cannot read"
                (["pg_catalog", "overlay"], [x, placing, from]) | sqlSyntax -> pure Overlay{string = x, placing, from, for = Nothing}
                (["pg_catalog", "overlay"], [x, placing, from, for]) | sqlSyntax -> pure Overlay{string = x, placing, from, for = Just for}
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
    Trim{modifier, chars, hasFrom, strings} -> do
        md <- maybe (pure mempty) (fmap (<> " ") . kw) modifier
        cd <- maybe (pure Nothing) (fmap Just . (child e.en >=> expr mode)) chars
        sd <- commaJoin <$> traverse (child e.en >=> expr mode) strings
        fromK <- kw "from"
        pure $
            md <> case (cd, hasFrom) of
                (Just c, _) -> c <+> fromK <+> sd
                (Nothing, True) -> fromK <+> sd
                (Nothing, False) -> sd
    Overlay{string, placing, from, for} -> do
        xd <- child e.en string >>= expr mode
        pd <- child e.en placing >>= expr mode
        fd <- child e.en from >>= expr mode
        forD <- maybe (pure mempty) (\n -> do k <- kw "for"; d <- child e.en n >>= expr mode; pure (" " <> k <+> d)) for
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
    | -- | The arguments, with @distinct@ or @all@ before them if written,
      -- and an @order by@ among them.
      Normal Quantifier [Pg.Node] [Pg.Node]
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
        pure (Normal (if f ^. Pg.aggDistinct then Distinct else if explicitAll then All else Unwritten) args sort)
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


-- | An aggregate's @distinct@ or @all@, or neither.
data Quantifier = Unwritten | Distinct | All


distinctPrefix :: Quantifier -> R D
distinctPrefix = \case
    Unwritten -> pure mempty
    Distinct -> (<> " ") <$> kw "distinct"
    All -> (<> " ") <$> kw "all"


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
    | not (has frameNonDefault) = pure Nothing
    | otherwise = do
        modeD <- kw if has frameRows then "rows" else if has frameGroups then "groups" else "range"
        start' <- bound startBound (w ^. Pg.maybe'startOffset)
        extent <-
            if has frameBetween
                then do
                    end' <- bound endBound (w ^. Pg.maybe'endOffset)
                    b <- kw "between"
                    a <- kw "and"
                    pure (b <+> start' <+> a <+> end')
                else pure start'
        exclusion <-
            if
                | has frameExcludeCurrentRow -> Just <$> kw "exclude current row"
                | has frameExcludeGroup -> Just <$> kw "exclude group"
                | has frameExcludeTies -> Just <$> kw "exclude ties"
                | otherwise -> pure Nothing
        pure (Just (spaceJoin (catMaybes [Just modeD, Just extent, exclusion])))
  where
    has flag = w ^. Pg.frameOptions .&. flag /= 0
    bound flags offset
        | has flags.unboundedPreceding = kw "unbounded preceding"
        | has flags.unboundedFollowing = kw "unbounded following"
        | has flags.currentRow = kw "current row"
        | has flags.offsetPreceding = do
            o <- maybe (unsupported "a frame offset") (top >=> expr MP) offset
            (o <+>) <$> kw "preceding"
        | has flags.offsetFollowing = do
            o <- maybe (unsupported "a frame offset") (top >=> expr MP) offset
            (o <+>) <$> kw "following"
        | otherwise = unsupported "a frame bound"


-- | The flags of a window definition's @frameOptions@ that say what its
-- frame is: PostgreSQL's @FRAMEOPTION_@ constants (nodes/parsenodes.h).
frameNonDefault, frameRows, frameGroups, frameBetween, frameExcludeCurrentRow, frameExcludeGroup, frameExcludeTies :: Int32
frameNonDefault = 0x1
frameRows = 0x4
frameGroups = 0x8
frameBetween = 0x10
frameExcludeCurrentRow = 0x8000
frameExcludeGroup = 0x10000
frameExcludeTies = 0x20000


-- | The flags that say what one end of a frame is.
data BoundFlags = BoundFlags
    { unboundedPreceding :: Int32
    , unboundedFollowing :: Int32
    , currentRow :: Int32
    , offsetPreceding :: Int32
    , offsetFollowing :: Int32
    }


startBound, endBound :: BoundFlags
startBound = BoundFlags{unboundedPreceding = 0x20, unboundedFollowing = 0x80, currentRow = 0x200, offsetPreceding = 0x800, offsetFollowing = 0x2000}
endBound = BoundFlags{unboundedPreceding = 0x40, unboundedFollowing = 0x100, currentRow = 0x400, offsetPreceding = 0x1000, offsetFollowing = 0x4000}


----------------------------------------------------------------------
-- Subqueries in expressions.
----------------------------------------------------------------------

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
    = -- | @coalesce@, @greatest@, @least@ or @nullif@: its keyword, its
      -- location, and its arguments.
      SubRows Text Int32 [Pg.Node]
    | -- | @cast(x as t)@: its location, the expression and the type.
      SubCast Int32 Pg.Node Pg.TypeName
    | -- | @extract@, @position@, @substring@, @trim@, @overlay@.
      SubSyntax
    | -- | @current_date@ and the like.
      SubValue


asSubexpr :: E -> R (Maybe Subexpr)
asSubexpr e
    | e.lv > 0 = pure Nothing
    | otherwise = case node e.en of
        Just (Pg.Node'CoalesceExpr c) -> pure (Just (SubRows "coalesce" (c ^. Pg.location) (c ^. Pg.args)))
        Just (Pg.Node'MinMaxExpr c) -> pure (Just (SubRows (if c ^. Pg.op == Pg.IS_GREATEST then "greatest" else "least") (c ^. Pg.location) (c ^. Pg.args)))
        Just (Pg.Node'AExpr a)
            | a ^. Pg.kind == Pg.AEXPR_NULLIF
            , Just l <- a ^. Pg.maybe'lexpr
            , Just r <- a ^. Pg.maybe'rexpr ->
                pure (Just (SubRows "nullif" (a ^. Pg.location) [l, r]))
        Just (Pg.Node'TypeCast tc)
            | Just tn <- tc ^. Pg.maybe'typeName
            , Just arg <- tc ^. Pg.maybe'arg -> do
                form <- castForm tc
                pure case form of
                    CastCall -> Just (SubCast (tc ^. Pg.location) arg tn)
                    _ -> Nothing
        Just (Pg.Node'FuncCall f) | not (hasExtras f) -> do
            call <- classifyCall f
            pure case call of
                Plain _ -> Nothing
                AtTimeZone _ _ -> Nothing
                _ -> Just SubSyntax
        Just (Pg.Node'SqlvalueFunction _) -> pure (Just SubValue)
        _ -> pure Nothing


-- | What an expression is, as the wrapped layout tells expressions apart:
-- in parentheses, under a @::@ cast, an ordinary call, one of
-- postgresql-syntax's common subexpressions, or something else.
data Shape
    = InParens E (Maybe Pg.A_Indirection)
    | UnderCast E Pg.TypeName
    | Application Pg.FuncCall
    | CommonSubexpr Subexpr
    | OtherShape


-- | An expression's 'Shape'. All four classifiers run, in this order,
-- whichever matches: they are not pure. 'inParens' and 'asColonCast' take
-- the child expression, which places the parentheses around it, and
-- 'asFuncApp' and 'asSubexpr' classify calls, which places a call's own
-- parenthesis. Stopping at the first match would leave some unplaced, or
-- change which refusal comes first.
shapeOf :: E -> R Shape
shapeOf e = do
    paren <- inParens e
    cast <- asColonCast e
    app <- asFuncApp e
    subexpr <- asSubexpr e
    pure case (paren, cast, app, subexpr) of
        (Just (inner, ind), _, _, _) -> InParens inner ind
        (_, Just (inner, tn), _, _) -> UnderCast inner tn
        (_, _, Just f, _) -> Application f
        (_, _, _, Just sub) -> CommonSubexpr sub
        _ -> OtherShape


isWrapEligible :: E -> R Bool
isWrapEligible e = do
    shape <- shapeOf e
    case shape of
        InParens inner _ -> isWrapEligible inner
        UnderCast inner _ -> isWrapEligible inner
        Application _ -> pure True
        CommonSubexpr SubValue -> pure False
        CommonSubexpr _ -> pure True
        OtherShape -> case node e.en of
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


-- | A call under any number of @::@ casts.
data CastedCall = CastedCall
    { call :: Pg.FuncCall
    , callExpr :: E
    , recast :: D -> R D
    -- ^ Put the casts back after the call's layout.
    }


-- | The call an expression is, maybe under @::@ casts.
topLevelFuncApp :: E -> R (Maybe CastedCall)
topLevelFuncApp e = do
    cast <- asColonCast e
    case cast of
        Just (inner, tn) -> do
            found <- topLevelFuncApp inner
            pure case found of
                Just c -> Just c{recast = \d -> do d' <- c.recast d; t <- typeName tn; pure (d' <> "::" <> t)}
                Nothing -> Nothing
        Nothing -> do
            app <- asFuncApp e
            pure ((\f -> CastedCall{call = f, callExpr = e, recast = pure}) <$> app)


funcAppArgCount :: Pg.FuncCall -> Int
funcAppArgCount f
    | f ^. Pg.aggStar = 0
    | otherwise = length (f ^. Pg.args)


-- | Arguments shaped like keys and values: at least four, an even number,
-- every other one from the first a string literal.
looksLikeKeyValuePairs :: E -> [Pg.Node] -> R Bool
looksLikeKeyValuePairs e args
    | n < keyValueMinArguments || odd n = pure False
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
        Just CastedCall{call = f, callExpr = fe} -> do
            params <- callParams f
            case params of
                Normal _ args _ -> looksLikeKeyValuePairs fe args
                _ -> pure False
        Nothing -> pure False


-- | "PgFmt.Render"'s: the fewest arguments a call laid out as key/value
-- pairs has.
keyValueMinArguments :: Int
keyValueMinArguments = 4


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
    shape <- shapeOf e
    case shape of
        InParens inner ind -> inParensWrapped e inner ind
        UnderCast inner tn -> do
            d <- exprWrapped inner
            t <- typeName tn
            pure (d <> "::" <> t)
        Application f -> funcApplicationWrapped e f
        CommonSubexpr sub -> subexprWrapped e sub
        OtherShape -> case node e.en of
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
    SubRows name at args -> do
        open <- tokAt (fromIntegral at) >>= nextParen
        _ <- expectParen open
        ds <- traverse (child e.en >=> exprMaybeWrap) args
        k <- kw name
        pure (k <> "(" <> nest indentSize (argRows ds) <> hardline <> ")")
    SubCast at arg tn -> do
        open <- tokAt (fromIntegral at) >>= nextParen
        _ <- expectParen open
        a <- child e.en arg >>= exprMaybeWrap
        t <- typeName tn
        k <- kw "cast"
        as <- kw "as"
        pure (k <> "(" <> nest indentSize (hardline <> a <+> as <+> t) <> hardline <> ")")
    _ -> expr MR e


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
    | e.lv == 0
    , Just (Pg.Node'SubLink s) <- node e.en
    , s ^. Pg.subLinkType == Pg.EXPR_SUBLINK =
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
        Just CastedCall{call = f, callExpr = fe, recast}
            | funcAppArgCount f >= funcWrapThreshold -> funcApplicationWrapped fe f >>= recast
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
        Just (Pg.Node'BoolExpr b)
            | e.lv == 0
            , b ^. Pg.boolop == Pg.AND_EXPR
            , first : rest <- b ^. Pg.args -> do
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
