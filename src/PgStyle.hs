-- | Format PostgreSQL.
--
-- * "PgStyle.File" formats whole files, statement by statement.
-- * "PgStyle.Haskell" formats the SQL in a Haskell file's hasql-th
--   quasiquotes.
-- * "PgStyle.Render" formats one DML statement with pgstyle's own layout, and
--   the SQL of hasql-th quasiquotes.
-- * "PgStyle.LibPgQuery" holds the libpg_query calls both rely on.
module PgStyle (
    module PgStyle.File,
    Dialect (..),
    FormatError (..),
    formatStatement,
    formatSql,
) where

import PgStyle.File
import PgStyle.Render (Dialect (..), FormatError (..), formatSql, formatStatement)

