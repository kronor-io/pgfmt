-- | pgfmt's own hopinion rules, which check the parts of docs/readability.md a
-- tool can decide. hopinion's built-in rules are left out: they are another
-- project's style.
module PgFmt.Lint (main, rules) where

import Hopinion (hopinionWith)
import Hopinion.Rule (Rule)
import PgFmt.Lint.Rule.NamedFields qualified as NamedFields
import PgFmt.Lint.Rule.NamedTuples qualified as NamedTuples
import PgFmt.Lint.Rule.SmallWhere qualified as SmallWhere


main :: IO ()
main = hopinionWith rules


rules :: [Rule]
rules =
    [ SmallWhere.rule
    , NamedTuples.rule
    , NamedFields.rule
    ]
