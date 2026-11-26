-- | Identifiers, constants and types.
module PgFmt.Layout.Names where

import Control.Monad (forM, unless, when, zipWithM)
import Control.Monad.Reader (asks)
import Data.Char (isAscii, isAsciiLower, isDigit)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import PgQuery qualified as Pg
import Prettyprinter (hcat, pretty, punctuate, (<+>))
import PgFmt.Layout.Monad


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

-- | The identifier a token spells, if it is one.
identText :: Tok -> Maybe Text
identText tk
    | tk.kind == Pg.IDENT, Just q <- T.stripPrefix "\"" tk.text = Just (T.replace "\"\"" "\"" (T.dropEnd 1 q))
    | tk.kind == Pg.IDENT || tk.isKeyword = Just (foldCase tk.text)
    | otherwise = Nothing
