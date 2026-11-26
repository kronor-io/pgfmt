module PgFmt.Layout.Dml where

import PgFmt.Layout.Monad (D, R)
import PgFmt.Layout.Parens (Sel)
import PgQuery qualified as Pg

selMaybeWrap :: Sel -> R D
selP :: Sel -> R D
selWrapped :: Sel -> R D
selectParens :: Int -> Pg.SelectStmt -> R Int
sortClause :: [Pg.Node] -> R D
