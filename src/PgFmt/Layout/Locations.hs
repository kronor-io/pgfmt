{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The source locations in a parse tree, found through protobuf
-- reflection.
module PgFmt.Layout.Locations where

import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.ProtoLens.Message (FieldAccessor (..), FieldDescriptor (..), FieldTypeDescriptor (..), Message (..), ScalarField (..))
import Data.Proxy (Proxy (..))
import Lens.Micro ((^.))
import PgQuery qualified as Pg
import Unsafe.Coerce (unsafeCoerce)


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
