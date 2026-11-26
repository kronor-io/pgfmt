-- | Expressions and subqueries where they occur, and the parentheses the
-- source has around them.
module PgFmt.Layout.Parens where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (gets, modify')
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Lens.Micro ((^.))
import PgQuery qualified as Pg
import PgFmt.Layout.Locations
import PgFmt.Layout.Monad


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



-- | A subquery: the select, and how many parentheses the source has around
-- it (at least one).
data Sel = Sel
    { stmt :: Pg.SelectStmt
    , parens :: Int
    }
