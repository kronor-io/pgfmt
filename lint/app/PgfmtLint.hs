-- | The pgfmt-lint executable: hopinion with pgfmt's own rules.
module PgfmtLint (main) where

import PgFmt.Lint qualified


main :: IO ()
main = PgFmt.Lint.main
