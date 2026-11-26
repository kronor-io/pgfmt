# Working on pgfmt

Read `docs/readability.md` before changing Haskell code. Apply its rules to the
code you change, and do not clean up unrelated code.

- Prefer explicit domain data, named stages and pure rules callable outside IO.
  Small local helpers, ordinary tuples and standard abstractions are fine.
- Decide what a construct is from the parse tree. Read the source tokens only for
  what the tree forgets: spelling, parentheses and comments.
- Never weaken the parse-tree check or the comment check. Refuse a statement
  rather than guess at it.
- When a change is meant to keep behaviour, say what it preserves and check it,
  as `docs/readability.md` describes. That covers output, refusals and comments.
- Run these inside `nix develop`, and report the commands you ran and anything
  you could not run:
  - `scripts/check fast` while working;
  - `scripts/check lint` when you change Haskell code;
  - `scripts/check full` before committing.
- Do not update expected outputs, add `[allow:...]` annotations or relax warnings
  just to make a check pass. An annotation needs a real reason; never write a new
  `pre-existing at adoption`.
- Do not add an abstraction only to satisfy a rule or a metric.

For a review of a change, follow `.claude/skills/readability-review/SKILL.md`.
