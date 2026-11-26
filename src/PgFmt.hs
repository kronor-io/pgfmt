-- | Format PostgreSQL.
--
-- * "PgFmt.File" formats whole files, statement by statement.
-- * "PgFmt.Haskell" formats the SQL in a Haskell file's hasql-th
--   quasiquotes.
-- * "PgFmt.Render" formats one DML statement with pgfmt's own layout, and the
--   SQL of hasql-th quasiquotes.
-- * "PgFmt.LibPgQuery" holds the libpg_query calls both rely on.
module PgFmt (
    module PgFmt.File,
    Dialect (..),
    FormatError (..),
    formatStatement,
    formatSql,
) where

import PgFmt.File
import PgFmt.Render (Dialect (..), FormatError (..), formatSql, formatStatement)

