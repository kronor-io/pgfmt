-- | The pgstyle-lint executable: hopinion with pgstyle's own rules.
module PgstyleLint (main) where

import PgStyle.Lint qualified


main :: IO ()
main = PgStyle.Lint.main
