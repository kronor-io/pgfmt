module PgStyle.Layout.Dml where

import PgQuery qualified as Pg
import PgStyle.Layout.Monad (D, R)
import PgStyle.Layout.Parens (Sel)


selMaybeWrap :: Sel -> R D
selP :: Sel -> R D
selWrapped :: Sel -> R D
selectParens :: Int -> Pg.SelectStmt -> R Int
sortClause :: [Pg.Node] -> R D
