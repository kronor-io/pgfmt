-- | pgstyle's own hopinion rules, which check the parts of docs/readability.md
-- a tool can decide. hopinion's built-in rules are left out: they are another
-- project's style.
module PgStyle.Lint (main, rules) where

import Hopinion (hopinionWith)
import Hopinion.Rule (Rule)
import PgStyle.Lint.Rule.NamedFields qualified as NamedFields
import PgStyle.Lint.Rule.NamedTuples qualified as NamedTuples
import PgStyle.Lint.Rule.SmallWhere qualified as SmallWhere


main :: IO ()
main = hopinionWith rules


rules :: [Rule]
rules =
    [ SmallWhere.rule
    , NamedTuples.rule
    , NamedFields.rule
    ]
