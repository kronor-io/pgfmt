-- | Selects, from clauses, and insert, update and delete.
module PgFmt.Layout.Dml where

import Control.Monad (forM, unless, when, zipWithM)
import Control.Monad.Reader (asks)
import Data.Foldable (toList)
import Data.IntSet qualified as IS
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.ProtoLens.Message (Message (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((&), (.~), (^.))
import PgFmt.Layout.Comments
import PgFmt.Layout.Expr
import PgFmt.Layout.Locations
import PgFmt.Layout.Monad
import PgFmt.Layout.Names
import PgFmt.Layout.Parens
import PgQuery qualified as Pg
import Prettyprinter (hardline, indent, nest, pretty, vsep, (<+>))


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
    remembered AroundSelect ls (selectParens' bound s ls)


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
                    Just m
                        | isParen
                        , not placed
                        , m > maxTok -> do
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
                Just m
                    | isParen
                    , m > maxTok -> do
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
operands :: Pg.SelectStmt -> R (Sel, Sel)
operands s = do
    l <- req "a set operation" (s ^. Pg.maybe'larg)
    r <- req "a set operation" (s ^. Pg.maybe'rarg)
    lp <- selectParens 0 l
    rp <- selectParens 0 r
    let bare x n = n == 0 && (isJust (x ^. Pg.maybe'withClause) || not (null (x ^. Pg.sortClause)) || isJust (x ^. Pg.maybe'limitCount) || isJust (x ^. Pg.maybe'limitOffset) || not (null (x ^. Pg.lockingClause)))
    when (bare l lp || bare r rp) (unsupported "a set operation operand pgfmt cannot read")
    pure (Sel{stmt = l, parens = lp}, Sel{stmt = r, parens = rp})


-- | @union all@ and the like, as written, and where.
setOperator :: Pg.SelectStmt -> Sel -> R (D, Int)
setOperator s r = do
    rFirst <- selectFirst r.stmt
    let k = rFirst - r.parens - 1
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
        (l, r) <- operands s
        ld <- if l.parens == 0 then selClause l.stmt else selWithParens l.stmt l.parens
        (op, opAt) <- setOperator s r
        rd <- if r.parens == 0 then selClause r.stmt else selWithParens r.stmt r.parens
        rAt <- attempt (subtract r.parens <$> selectFirst r.stmt)
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
            written <- allWritten s
            if written then (sel <+>) <$> kw "all" else pure sel
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


-- | Whether a select without @distinct@ has @all@ written before its
-- targets.
allWritten :: Pg.SelectStmt -> R Bool
allWritten s = case s ^. Pg.targetList of
    t : _ | Just (Pg.Node'ResTarget rt) <- node t -> tokAt (fromIntegral (rt ^. Pg.location)) >>= \i -> tokIs (i - 1) "all"
    _ -> pure False


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
                | d == 0
                , identText tk == Just name -> do
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


depthBetween :: Int -> Int -> R Int
depthBetween a b =
    sum <$> forM [a .. b - 1] \i -> do
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
        (l, r) <- operands s
        ld <- if l.parens == 0 then selClauseInline l.stmt else selInline l
        (op, _) <- setOperator s r
        rd <- if r.parens == 0 then selClauseInline r.stmt else selInline r
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
        (l, r) <- operands s
        ld <- if l.parens == 0 then selClauseP l.stmt else selP l
        (op, _) <- setOperator s r
        rd <- if r.parens == 0 then selClauseP r.stmt else selP r
        pure (ld <+> op <+> rd)
    | not (null (s ^. Pg.valuesLists)) = valuesP s
    | otherwise = do
        when (isJust (s ^. Pg.maybe'intoClause)) (unsupported "select into")
        sel <- kw "select"
        targets <- traverse targetElP (s ^. Pg.targetList)
        quantifier <- case s ^. Pg.distinctClause of
            [] -> do
                written <- allWritten s
                if written then Just <$> kw "all" else pure Nothing
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
-- | "PgFmt.Render"'s: the most columns a function alias or an insert keeps
-- on one line.
funcAliasInlineColumns, insertInlineColumns :: Int
funcAliasInlineColumns = 3
insertInlineColumns = 3


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
    remembered AroundJoin ls0 (joinParens' n)


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
                Just m
                    | isParen
                    , not placed
                    , m > maxTok -> do
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
    | length (a ^. Pg.colnames) > funcAliasInlineColumns = do
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
                if length ds <= insertInlineColumns
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
