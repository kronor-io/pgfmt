-- | DDL, in a style of its own: create table, view, index, function and
-- trigger, alter table, grant and revoke.
--
-- The keywords of a clause are the source's own words (@temp@ or
-- @temporary@, an explicit @on delete no action@), in the keyword case;
-- names, types and expressions come from the tree, as in DML. Anything
-- else (storage options pgfmt does not know, @exclude@, ...) is not laid
-- out.
module PgFmt.Layout.Ddl where

import Control.Monad (forM, forM_, unless, when, zipWithM)
import Control.Monad.Reader (asks)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (zip4)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Micro ((&), (.~), (^.))
import PgQuery qualified as Pg
import Prettyprinter (hardline, nest, pretty, vsep, (<+>))
import PgFmt.Layout.Monad
import PgFmt.Layout.Comments
import PgFmt.Layout.Parens
import PgFmt.Layout.Names
import PgFmt.Layout.Expr
import PgFmt.Layout.Dml


-- | The statement's last token, before any semicolon.
lastTok :: R Int
lastTok = do
    ts <- asks (.toks)
    case IM.lookupMax ts of
        Just (i, t) | t.text == ";" -> pure (i - 1)
        Just (i, _) -> pure i
        Nothing -> unsupported "an empty statement"


data Piece = Word | Open | Call | Glue | Close | Dot


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
        (isJust (c ^. Pg.maybe'ofTypename) || (not (null (c ^. Pg.inhRelations)) && isNothing (c ^. Pg.maybe'partbound)))
        (unsupported "a create table pgfmt does not lay out")
    (name, relAt, open) <- relationAt rv
    header <- wordsRun 0 relAt
    if isJust (c ^. Pg.maybe'partbound)
        then partitionOf c (header <+> name) open
        else tableBody c (header <+> name) open


-- | @create table t partition of p for values ...@: each clause on a line
-- of its own, at the statement's column.
partitionOf :: Pg.CreateStmt -> D -> Int -> R D
partitionOf c head' at = do
    when
        (not (null (c ^. Pg.tableElts)) || isJust (c ^. Pg.maybe'partspec))
        (unsupported "a create table pgfmt does not lay out")
    parent <- case c ^. Pg.inhRelations of
        [p] | Just (Pg.Node'RangeVar rv) <- node p -> pure rv
        _ -> unsupported "a create table pgfmt cannot read"
    isPartition <- (&&) <$> tokIs at "partition" <*> tokIs (at + 1) "of"
    (pname, pAt, pAfter) <- relationAt parent
    unless (isPartition && pAt == at + 2) (unsupported "a create table pgfmt cannot read")
    w <- wordsRun at pAt
    end <- (+ 1) <$> lastTok
    unless (pAfter < end) (unsupported "a create table pgfmt cannot read")
    bound <- looseRun pAfter end
    b1 <- brk mempty at
    b2 <- brk mempty pAfter
    pure (head' <> b1 <> w <+> pname <> b2 <> bound)


-- | A table's columns and constraints, in the parentheses at @open@, and
-- what follows them.
tableBody :: Pg.CreateStmt -> D -> Int -> R D
tableBody c head' open = do
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
    pure (head' <+> body <> tl)


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
                || (not (T.null (x ^. Pg.indexname)) && not (null (x ^. Pg.keys)))
                || not (T.null (x ^. Pg.indexspace))
                || isJust (x ^. Pg.maybe'whereClause)
                || x ^. Pg.withoutOverlaps
            )
            (unsupported "a key pgfmt does not lay out")
        if
            | not (T.null (x ^. Pg.indexname)) -> do
                -- @primary key using index i@: a long one breaks before
                -- @using@.
                usingAt <- findUsing a
                w <- wordsRun a usingAt
                u <- wordsRun usingAt (usingAt + 2)
                nm <- identAt (usingAt + 2) (x ^. Pg.indexname)
                t <- tailRun (usingAt + 3) b
                let rest = u <+> nm <> t
                pure
                    if before + len (w <+> rest) <= foreignKeyInlineThreshold
                        then w <+> rest
                        else nest indentSize (w <> hardline <> rest)
            | null (x ^. Pg.keys) -> wordsRun a b
            | otherwise -> do
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
    findUsing j
        | j >= b = unsupported "a key pgfmt cannot read"
        | otherwise = do
            u <- tokIs j "using"
            i <- tokIs (j + 1) "index"
            if u && i then pure j else findUsing (j + 1)


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
        ( not (T.null (ix ^. Pg.tableSpace))
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
    between <- indexTail ix (close + 1) (fromMaybe end whereAt)
    wh <- traverse indexWhere (ix ^. Pg.maybe'whereClause)
    let onLine = onK <+> rel <> usingD <+> "(" <> commaJoin cols <> ")" <> between
    onBreak <- brk mempty (relAt - 1)
    whBreak <- brkAt mempty whereAt
    pure (header <> nameD <> nest indentSize (onBreak <> onLine <> maybe mempty (whBreak <>) wh))


-- | What follows an index's columns, before its @where@: @include (a,
-- b)@, @nulls not distinct@, @with (fastupdate = off)@; with a space
-- before it, if there is any.
indexTail :: Pg.IndexStmt -> Int -> Int -> R D
indexTail ix from to = do
    included <- forM (ix ^. Pg.indexIncludingParams) \p -> case node p of
        Just (Pg.Node'IndexElem el) | isNothing (el ^. Pg.maybe'expr) -> pure (el ^. Pg.name)
        _ -> unsupported "an index pgfmt does not lay out"
    let go j
            | j >= to = pure []
            | otherwise = do
                w <- tokL j
                case w of
                    "include" | not (null included) -> do
                        k <- kw "include"
                        (l, after) <- nameList (j + 1) included
                        ((k <+> l) :) <$> go after
                    "with" | not (null (ix ^. Pg.options)) -> do
                        k <- kw "with"
                        close <- expectParen (j + 1)
                        o <- relOptions (ix ^. Pg.options)
                        ((k <+> o) :) <$> go (close + 1)
                    _ -> do
                        d <- wordsRun j (j + 1)
                        (d :) <$> go (j + 1)
    ds <- go from
    pure (mconcat [" " <> d | d <- ds])


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
    -- @alter constraint c deferrable@: the name is the constraint's.
    Pg.AT_AlterConstraint -> case def of
        Just (Pg.Node'AtalterConstraint x) -> do
            nameAt <- nameToken
            unless (nameAt < e) cannot
            w <- wordsRun s nameAt
            nm <- identAt nameAt (x ^. Pg.conname)
            t <- tailRun (nameAt + 1) e
            pure (w <+> nm <> t)
        _ -> cannot
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
            Just (Pg.Node'List l) -> relOptions (l ^. Pg.items)
            _ -> cannot
        t' <- tailRun (close + 1) e
        pure (w <+> opts <> t')
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


-- | The tokens from @a@ to before @b@, as written but for keywords, in the
-- keyword case: a function's clauses (@language plpgsql@, @set search_path
-- = public@, @as 'obj', 'sym'@).
looseRun :: Int -> Int -> R D
looseRun a b = do
    pieces <- forM [a .. b - 1] \j -> do
        t <- tokM j
        case t of
            Just tk
                | tk.text == "(" -> do
                    _ <- expectParen j
                    -- Right after a name, as in @f(bigint)@, if the source
                    -- has it there.
                    prev <- if j > a then tokM (j - 1) else pure Nothing
                    let glued = case prev of
                            Just p -> p.text `notElem` ["(", ")", ","] && p.start + BS.length (TE.encodeUtf8 p.text) == tk.start
                            Nothing -> False
                    pure (if glued then Call else Open, "(")
                | tk.text `elem` [")", ","] -> pure (Close, pretty tk.text)
                | tk.text == "." -> pure (Dot, ".")
                | tk.isKeyword && tk.kind /= Pg.IDENT -> (Word,) <$> kw (T.toLower tk.text)
                | otherwise -> pure (Word, raw tk.text)
            Nothing -> unsupported "a clause pgfmt cannot read"
    pure case pieces of
        [] -> mempty
        (k, d) : rest -> d <> go k rest
  where
    go _ [] = mempty
    go prev ((k, d) : rest) = sep prev k <> d <> go k rest
    sep Open _ = mempty
    sep Call _ = mempty
    sep Dot _ = mempty
    sep _ Close = mempty
    sep _ Dot = mempty
    sep _ Call = mempty
    sep _ _ = " "


-- | @create function@ and @create procedure@: the arguments on the first
-- line, or one per line if they do not fit; each clause before the body on
-- a line of its own, the last ending in @as@; the body from the start of
-- the line after, as written; the clauses after it on its last line.
createFunction :: (Pg.Node -> R D) -> Pg.CreateFunctionStmt -> R D
createFunction statement f = do
    names <- strsOf (f ^. Pg.funcname)
    -- The name: the first identifier after the words that start the
    -- statement.
    nameAt <- firstName 0
    header <- wordsRun 0 nameAt
    name <- qualNameAt nameAt names
    let open = nameAt + 2 * length names - 1
    close <- expectParen open
    params <- forM (f ^. Pg.parameters) \p -> case node p of
        Just (Pg.Node'FunctionParameter fp) -> pure fp
        _ -> unsupported "a function parameter pgfmt cannot read"
    let isTable fp = fp ^. Pg.mode == Pg.FUNC_PARAM_TABLE
        (tableCols, args) = (filter isTable params, filter (not . isTable) params)
    argDs <- paramList args (open + 1) close
    end <- (+ 1) <$> lastTok
    -- The clauses, in source order: @returns@ (right after the arguments,
    -- if there), then each option.
    opts <- forM (f ^. Pg.options) \o -> case node o of
        Just (Pg.Node'DefElem de) -> (de,) <$> tokAt (fromIntegral (de ^. Pg.location))
        _ -> unsupported "a function option pgfmt cannot read"
    standardAt <- case f ^. Pg.maybe'sqlBody of
        Just _ -> Just <$> standardBodyStart (close + 1)
        Nothing -> pure Nothing
    let optStarts = map snd opts
        firstOpt = foldr min (fromMaybe end standardAt) optStarts
    returnsD <-
        if close + 1 < firstOpt
            then do
                isReturns <- tokIs (close + 1) "returns"
                unless isReturns (unsupported "a function pgfmt cannot read")
                Just <$> returnsClause f tableCols (close + 1) firstOpt
            else pure Nothing
    -- Where the body starts: @as@, or a SQL-standard body after every
    -- clause.
    body <- case (f ^. Pg.maybe'sqlBody, [s | (de, s) <- opts, de ^. Pg.defname == "as"]) of
        (Nothing, [s]) -> pure (QuotedBody s)
        (Just b, []) | Just at <- standardAt -> pure (StandardBody at b)
        _ -> unsupported "a function without a body pgfmt can place"
    let bodyAt = case body of
            QuotedBody at -> at
            StandardBody at _ -> at
        -- A SQL-standard body ends the last clause; @as@ is a clause itself.
        ends = case body of
            StandardBody{} -> map (min bodyAt) (drop 1 optStarts ++ [end])
            QuotedBody{} -> drop 1 optStarts ++ [end]
        clauses = zipWith Clause optStarts ends
        before = [c | c <- clauses, c.start < bodyAt]
        after = [c | c <- clauses, c.start > bodyAt]
        asEnd = fromMaybe end (listToMaybe [c.end | c <- clauses, c.start == bodyAt])
    beforeDs <- forM before \c -> do
        d <- looseRun c.start c.end
        b <- brk mempty c.start
        pure (b <> d)
    returnsLine <- case returnsD of
        Nothing -> pure mempty
        Just d -> (<> d) <$> brk mempty (close + 1)
    argsD <- itemList functionHeaderThreshold (header <+> name) argDs close
    let clauseLines = returnsLine <> mconcat beforeDs
    bodyD <- case body of
        StandardBody at b -> do
            bb <- brk mempty at
            d <- standardBody statement b at end
            pure (bb <> d)
        QuotedBody at -> quotedBody at asEnd after
    pure (header <+> name <> argsD <> nest indentSize clauseLines <> bodyD)
  where
    firstName j = do
        t <- tokM j
        case t of
            Just tk | tk.kind == Pg.IDENT || (tk.isKeyword && T.toLower tk.text `notElem` ["create", "or", "replace", "function", "procedure"]) -> pure j
            Just _ -> firstName (j + 1)
            Nothing -> unsupported "a function pgfmt cannot read"


-- | Where a function's body is: in quotes after @as@ at this token, or a
-- SQL-standard one starting at this token.
data FunctionBody
    = QuotedBody Int
    | StandardBody Int Pg.Node


-- | A clause of a function, from its first token to before @end@.
data Clause = Clause
    { start :: Int
    , end :: Int
    }


-- | A quoted body after @as@ at this token, up to before @asEnd@, and the
-- clauses after it. The body starts the line after @as@, as written; its
-- opening quote lines up with the closing one, when that starts a line of
-- its own. The clauses after it follow it on its last line; with a comment
-- among them, each after the first on a line of its own.
quotedBody :: Int -> Int -> [Clause] -> R D
quotedBody asAt asEnd after = do
    asK <- kw "as"
    bodyBrk <- brk mempty (asAt + 1)
    col <- asks (.column)
    bodyTok <- tokM (asAt + 1)
    let closeCol = case bodyTok of
            Just tk | asEnd == asAt + 2, Just tag <- dollarTag tk.text -> do
                let lastLine = snd (T.breakOnEnd "\n" tk.text)
                    (spaces, rest) = T.span (== ' ') lastLine
                if T.isInfixOf "\n" tk.text && rest == tag then Just (T.length spaces) else Nothing
            _ -> Nothing
        openIndent = maybe mempty (\c -> pretty (T.replicate (max 0 (c - col)) " ")) closeCol
    body <- looseRun (asAt + 1) asEnd
    afterDs <- forM after \c -> looseRun c.start c.end
    commented <- or <$> forM (drop 1 after) \c -> not . null <$> gapBefore c.start
    rest <-
        if commented
            then forM (zip (drop 1 after) (drop 1 afterDs)) \(c, d) -> (<> d) <$> brk mempty c.start
            else pure [" " <> d | d <- drop 1 afterDs]
    let firstAfter = maybe mempty (" " <>) (listToMaybe afterDs)
        clausesAfter = if commented then nest indentSize (mconcat rest) else mconcat rest
    pure (" " <> asK <> bodyBrk <> openIndent <> body <> firstAfter <> clausesAfter)


-- | Where a SQL-standard body starts: @begin atomic@ or @return@, at or
-- after this token.
standardBodyStart :: Int -> R Int
standardBodyStart j = do
    w <- tokL j
    w1 <- tokL (j + 1)
    t <- tokM j
    case t of
        Nothing -> unsupported "a function body pgfmt cannot find"
        Just _
            | w == "return" || (w == "begin" && w1 == "atomic") -> pure j
            | w == "(" -> do
                m <- matchOf j
                maybe (unsupported "an unclosed parenthesis") (standardBodyStart . (+ 1)) m
            | otherwise -> standardBodyStart (j + 1)


-- | A SQL-standard body, from token @at@ to before @end@: @return@ and its
-- expression, or @begin atomic@, each statement laid out on the lines
-- after it, indented, and @end@.
standardBody :: (Pg.Node -> R D) -> Pg.Node -> Int -> Int -> R D
standardBody statement b at end = case node b of
    Just (Pg.Node'ReturnStmt r) -> do
        k <- kw "return"
        ex <- req "a return" (r ^. Pg.maybe'returnval)
        d <- ddlExpr ex
        pure (k <+> d)
    Just (Pg.Node'List outer) | [inner] <- outer ^. Pg.items, Just (Pg.Node'List l) <- node inner -> do
        let stmts = l ^. Pg.items
        isEnd <- tokIs (end - 1) "end"
        unless isEnd (unsupported "a function body pgfmt cannot read")
        -- Each statement starts after @atomic@ or after a semicolon; one
        -- right after @atomic@ is kept.
        semiFirst <- tokIs (at + 2) ";"
        let first = if semiFirst then at + 3 else at + 2
        semis <- semicolons first (end - 1)
        let starts = first : map (+ 1) semis
        unless (length semis == length stmts) (unsupported "a function body pgfmt cannot read")
        k <- (<> (if semiFirst then ";" else mempty)) <$> kw "begin atomic"
        ds <- forM (zip stmts starts) \(st, s) -> do
            d <- statement st
            br <- brk mempty s
            pure (br <> d <> ";")
        endBrk <- brk mempty (end - 1)
        e <- kw "end"
        pure (k <> nest indentSize (mconcat ds) <> endBrk <> e)
    _ -> unsupported "a function body pgfmt does not lay out"
  where
    semicolons j stop
        | j >= stop = pure []
        | otherwise = do
            w <- tokL j
            case w of
                ";" -> (j :) <$> semicolons (j + 1) stop
                "(" -> do
                    m <- matchOf j
                    maybe (unsupported "an unclosed parenthesis") (\c -> semicolons (c + 1) stop) m
                _ -> semicolons (j + 1) stop


-- | A dollar-quoted string's tag, as in @$@ or @$body$@.
dollarTag :: Text -> Maybe Text
dollarTag t = case T.uncons t of
    Just ('$', rest) | (tag, r) <- T.breakOn "$" rest, not (T.null r) -> Just ("$" <> tag <> "$")
    _ -> Nothing


-- | The longest @create function name(args)@ line that keeps its arguments
-- on it.
functionHeaderThreshold :: Int
functionHeaderThreshold = 100


-- | A function's parameters, between the parentheses at these tokens, with
-- the token each starts at.
paramList :: [Pg.FunctionParameter] -> Int -> Int -> R [(Int, D)]
paramList ps from close = do
    starts <- traverse (tokAt . fromIntegral . (^. Pg.location)) ps
    case starts of
        [] -> unless (from == close) (unsupported "a function pgfmt cannot read")
        s0 : _ -> unless (s0 == from) (unsupported "a function pgfmt cannot read")
    let ends = map (subtract 1) (drop 1 starts) ++ [close]
    forM (zip3 ps starts ends) \(p, s, e) -> (s,) <$> param p s e


-- | A parenthesized list (of parameters, of columns) after @lead@: on one
-- line if it fits in @width@ and has no comments, otherwise one item per
-- line with the comma at the end, the comments in their places.
itemList :: Int -> D -> [(Int, D)] -> Int -> R D
itemList width lead items close = do
    commented <- or <$> mapM (fmap (not . null) . gapBefore) (map fst items ++ [close])
    let inline = "(" <> commaJoin (map snd items) <> ")"
        n = length items
    if null items && not commented
        then pure "()"
        else
            if not commented && len (lead <> inline) <= width
                then pure inline
                else do
                    ls <- forM (zip [0 :: Int ..] items) \(i, (q, d)) -> do
                        b <- brk mempty q
                        pure (b <> d <> (if i < n - 1 then "," else mempty))
                    cs <- gapBefore close
                    let (trailing, own) = span (not . (.ownLine)) cs
                        endComments =
                            mconcat [" " <> raw x.ctext | x <- trailing]
                                <> mconcat [hardline <> (if x.blankBefore then hardline else mempty) <> raw x.ctext | x <- own]
                    pure ("(" <> nest indentSize (mconcat ls <> endComments) <> hardline <> ")")


-- | One parameter, from token @s@ to before @e@: its mode, name, type and
-- default.
param :: Pg.FunctionParameter -> Int -> Int -> R D
param p s e = do
    w <- tokL s
    let explicitMode = w `elem` ["in", "out", "inout", "variadic"]
    modeD <- if explicitMode then Just <$> kw w else pure Nothing
    let nameAt = if explicitMode then s + 1 else s
    nameD <-
        if T.null (p ^. Pg.name)
            then pure Nothing
            else Just <$> identAt nameAt (p ^. Pg.name)
    tn <- req "a function parameter" (p ^. Pg.maybe'argType)
    ty <- typeName tn
    def <- case p ^. Pg.maybe'defexpr of
        Nothing -> pure Nothing
        Just ex -> do
            -- @default@ or @=@, the first such token after the type.
            tyAt <- tokAt (fromIntegral (tn ^. Pg.location))
            k <- findDefault tyAt
            kd <- if k == "=" then pure "=" else kw "default"
            d <- top ex >>= expr MR
            pure (Just (kd <+> d))
    pure (spaceJoin (catMaybes [modeD, nameD, Just ty, def]))
  where
    findDefault j
        | j >= e = unsupported "a parameter default pgfmt cannot read"
        | otherwise = do
            w <- tokL j
            if w `elem` ["default", "="] then pure w else findDefault (j + 1)


-- | @returns type@, @returns setof type@ or @returns table (columns)@, from
-- token @s@ to before @e@.
returnsClause :: Pg.CreateFunctionStmt -> [Pg.FunctionParameter] -> Int -> Int -> R D
returnsClause f tableCols s e = do
    k <- kw "returns"
    isTable <- tokIs (s + 1) "table"
    if isTable
        then do
            t <- kw "table"
            close <- expectParen (s + 2)
            unless (close == e - 1) (unsupported "a returns table pgfmt cannot read")
            cols <- paramList tableCols (s + 3) close
            l <- itemList functionHeaderThreshold (k <+> t <> " ") cols close
            pure (k <+> t <+> l)
        else do
            tn <- req "a function's return type" (f ^. Pg.maybe'returnType)
            setof <- tokIs (s + 1) "setof"
            let tyStart = if setof then s + 2 else s + 1
            at <- tokM tyStart >>= maybe (unsupported "a return type pgfmt cannot read") (pure . (.start))
            ty <- typeName (tn & Pg.setof .~ False & Pg.location .~ fromIntegral at)
            sd <- if setof then (<> " ") <$> kw "setof" else pure mempty
            pure (k <+> sd <> ty)


-- | @create trigger@: the trigger's name on the first line, then each
-- clause on a line of its own, at the statement's column: @after insert@,
-- @on t@, @referencing ...@, @for each row@, @when (...)@, @execute
-- function f()@.
createTrigger :: Pg.CreateTrigStmt -> R D
createTrigger t = do
    trigAt <- findTrigger 0
    header <- wordsRun 0 (trigAt + 1)
    let nameAt = trigAt + 1
    name <- identAt nameAt (t ^. Pg.trigname)
    end <- (+ 1) <$> lastTok
    starts <- clauseStarts (nameAt + 1) end
    case starts of
        s0 : _ | s0 == nameAt + 1 -> pure ()
        _ -> unsupported "a trigger pgfmt cannot read"
    let ends = drop 1 starts ++ [end]
    ds <- forM (zip starts ends) \(s, e) -> do
        w <- tokL s
        d <- case w of
            "when" -> do
                close <- expectParen (s + 1)
                unless (close == e - 1) (unsupported "a trigger pgfmt cannot read")
                c <- req "a trigger's condition" (t ^. Pg.maybe'whenClause)
                k <- kw "when"
                (k <+>) <$> checkCondition c
            "execute" -> do
                names <- strsOf (t ^. Pg.funcname)
                ws <- wordsRun s (s + 2)
                f <- qualNameAt (s + 2) names
                let open = s + 2 + 2 * length names - 1
                close <- expectParen open
                unless (close == e - 1) (unsupported "a trigger pgfmt cannot read")
                args <- looseRun (open + 1) close
                pure (ws <+> f <> "(" <> args <> ")")
            _ -> looseRun s e
        b <- brk mempty s
        pure (b <> d)
    pure (header <+> name <> mconcat ds)
  where
    findTrigger j = do
        w <- tokL j
        case w of
            "trigger" -> pure j
            _ | w `elem` ["create", "or", "replace", "constraint"] -> findTrigger (j + 1)
            _ -> unsupported "a trigger pgfmt cannot read"
    -- Where each clause starts: the top-level words that start one.
    clauseStarts j b
        | j >= b = pure []
        | otherwise = do
            w <- tokL j
            prev <- tokL (j - 1)
            next <- tokL (j + 1)
            let starts =
                    w `elem` ["before", "after", "instead", "on", "from", "referencing", "for", "when", "execute"]
                        || (w == "not" && next == "deferrable")
                        || (w == "deferrable" && prev /= "not")
                        || (w == "initially" && prev /= "deferrable")
            rest <-
                if w == "("
                    then matchOf j >>= maybe (unsupported "an unclosed parenthesis") (\c -> clauseStarts (c + 1) b)
                    else clauseStarts (j + 1) b
            pure (if starts then j : rest else rest)


-- | The longest @grant@ or @revoke@ that stays on one line.
grantInlineThreshold :: Int
grantInlineThreshold = 80


-- | @grant@ and @revoke@, of privileges (@on@ objects) or of roles: on one
-- line if short; otherwise the privileges on the first line, then @on@ the
-- objects and @to@ (or @from@) the roles, each on a line of its own.
grantStmt :: Bool -> R D
grantStmt hasOn = do
    end <- (+ 1) <$> lastTok
    onAt <- if hasOn then Just <$> (firstTop ["on"] 1 end >>= req "a grant") else pure Nothing
    toAt <- firstTop ["to", "from"] (maybe 1 (+ 1) onAt) end >>= req "a grant"
    let starts = 0 : catMaybes [onAt, Just toAt]
        ends = drop 1 starts ++ [end]
    ps <- forM (zip starts ends) (uncurry looseRun)
    cmts <- commentsBetween 0 (end - 1)
    let inline = spaceJoin ps
    case ps of
        first : rest | cmts || len inline > grantInlineThreshold -> do
            bs <- forM (drop 1 starts) (brk mempty)
            pure (first <> nest indentSize (mconcat (zipWith (<>) bs rest)))
        _ -> pure inline
  where
    -- The first of these words at the top level from @j@ to before @b@.
    firstTop ws j b
        | j >= b = pure Nothing
        | otherwise = do
            w <- tokL j
            if
                | w `elem` ws -> pure (Just j)
                | w == "(" -> matchOf j >>= maybe (unsupported "an unclosed parenthesis") (\c -> firstTop ws (c + 1) b)
                | otherwise -> firstTop ws (j + 1) b


-- | Storage options, @(fillfactor = 70, fastupdate = off)@, in their
-- parentheses, which the caller has placed.
relOptions :: [Pg.Node] -> R D
relOptions items = do
    opts <- forM items \o -> case node o of
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
                        Just vt | eq, vt.isKeyword, vt.kind /= Pg.IDENT -> (\k -> nm <> " = " <> k) <$> kw (T.toLower vt.text)
                        Just vt | eq, vt.kind == Pg.IDENT, not ("\"" `T.isPrefixOf` vt.text) -> pure (nm <> " = " <> raw vt.text)
                        _ -> cannot
        _ -> cannot
    pure ("(" <> commaJoin opts <> ")")
  where
    cannot :: R a
    cannot = unsupported "a storage option pgfmt cannot read"
