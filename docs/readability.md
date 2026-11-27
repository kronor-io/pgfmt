# Readable code in pgstyle

This is how pgstyle's code is meant to read, and how that is checked. The rules come
from sqiff's, where they came out of refactoring code that reviewers found
unreadable. The examples here are pgstyle's own, from `git log`.

They are review rules, not laws, and each says when not to apply it. A change
applies them to the code it touches. It does not clean up unrelated code on the
way.

## General rules

### R1. Show an operation's stages as named steps

A function that finds, decides and renders in one body makes the reader hold all
of it at once. Split it so that its body reads like a table of contents over
named functions with signatures.

```haskell
-- before: one body found the code, its line and column, formatted the PL/pgSQL
-- body, reparsed it, laid it out and built the replacement
formatOne opts source toks stmt = case [t | t <- toks, ...] of ...

-- after
formatOne opts source toks stmt =
    case statementCode source toks stmt of
        Nothing -> ...
        Just code -> do
            prepared <- formatBody opts code.text stmt.raw
            ...
            laid <- layOut opts code.column p.sql
```

A large `where` block is the symptom. The problem is the hidden dependencies and
the several jobs in one place. Do not split a short, linear operation, or extract
pieces you can only name `go2` or `process`. Small local helpers are fine.

### R2. Name bundles whose positions carry meaning

A tuple whose elements are told apart only by position becomes a record. So does
a list of positional arguments threaded through a recursion.

```haskell
-- before
operands :: Pg.SelectStmt -> R (Pg.SelectStmt, Int, Pg.SelectStmt, Int)
runBackend :: ... -> IO (Either Outcome (Backend, Bool, Text))

-- after
operands :: Pg.SelectStmt -> R (Sel, Sel)
runBackend :: ... -> IO (Either Outcome Laid)
```

There are more examples:

- the Haskell scanner's offset, line and column became a `Cursor`;
- a window frame bound's five masks became `BoundFlags`.

Leave conventional pairs, map entries and short local intermediates alone. A
record that grows into a `Context` holding everything hides dependencies as
well as a closure does.

### R3. Record a fact where it becomes known

Sometimes code works out what something means from incidental data: a magic
number, a substring of rendered output, a message prefix. Store the fact
explicitly where it is first known instead.

```haskell
-- before: tags 0 to 4 in a memo key said what was asked, and 4 was a boolean
-- run's group, stored as a count of 1 under a key with two zeros
memo :: Map.Map (Int, Int, Int, Int) Int
mine <- gets (Map.member (4, p, 0, 0) . (.memo))

-- after
parenCounts :: Map.Map (ParenQuestion, NodeSpan) Int
boolGroups :: IS.IntSet
```

A `Maybe` that means something other than absence becomes a sum type, as a call's
`Maybe Bool` (`Just True` for `distinct`) became
`Quantifier = Unwritten | Distinct | All`.

This does not forbid reading text. pgstyle reads the source's tokens for what
the parse tree forgets: spelling, parentheses and comments. That is the input.
The rule is about reading meaning back out of pgstyle's own output.

### R4. Decide first, present after

When a decision means something on its own, make it a value, and render that
value in a separate function. For example, `FunctionBody` (`QuotedBody` or
`StandardBody`) says where a function's body is, and `quotedBody` and
`standardBody` lay each form out.

A one-line layout with no reusable decision behind it does not need a type.

### R5. Keep pure rules callable outside the IO

Policy buried in IO can only be tested by running it. Pull it out as a pure
function that the IO calls, as `statementCode` is pure and `formatOne` calls it.
Do not split code where the order of effects is the point.

### R6. Name the questions the code keeps asking

Rows each on a line of their own, after their comments, with a comma after all
but the last, were written out in three places in the DDL layout. That is one
question, and `commaRows` answers it. Name a question like this before reaching
for a new data structure.

### R7. Write down the rules a change relies on

A refactor that depends on an invariant states it beside the code that produces
the value. Two examples:

- placing parentheses must give the same answer when asked again;
- a token index is inclusive in one place and exclusive in another.

Comments explain contracts, order and surprises. They do not restate the code,
and not every helper needs one.

## Haskell

### H1. Build records with many fields by field name

```haskell
-- before
Trim modifier (Just chars) True [s]
-- after
Trim{modifier, chars = Just chars, hasFrom = True, strings = [s]}
```

A constructor with two or three obvious fields can stay positional. A test
helper with defaults must not hide the field the test is about.

### H2. Give each real stage a top-level signature and explicit inputs

Keep small helpers local when their place explains them, and keep mutually
recursive workers together.

### H3. Plain data and functions before abstraction machinery

Reach for a record, a sum type or a named function first. A class hierarchy or
a generic framework has to justify itself by making its callers simpler.
Ordinary `traverse`, `foldMap` and `Semigroup` are welcome.

The parse tree comes from protobuf, read through `microlens`, and
`PgStyle.Layout.Locations` walks it by reflection. Neither is a model for
pgstyle's own types.

## pgstyle

- **Output is a contract.** A refactor keeps the formatted text byte for byte
  unless the change is meant to alter it. This covers Kronor's migrations in
  both keyword cases, its Haskell files, and every test.
- **Refuse rather than guess.** Every statement pgstyle rewrites must still
  parse to the same tree, and every comment must come out once, in order. A
  construct the layout does not know is refused with `unsupported`. Never weaken
  those checks to let a change through.
- **Meaning from the tree, spelling from the tokens.** Decide what a construct is
  from libpg_query's parse tree. Read the source tokens only for what the tree
  forgets, which is how it was spelled, its parentheses and its comments.
- **Every parenthesis is placed once.** Code that places the parentheses around
  a node, which makes them that node's, goes through `remembered`, so that asking
  again gives the same answer. Code that only reads the source's parentheses,
  such as `selectParensPeek`, places nothing and needs no memo.
- **"PgStyle.Layout" follows "PgStyle.Render".** The DML layout is
  "PgStyle.Render"'s, function for function, and is named after it. Change both
  together, or say why they differ.
- **Name each threshold.** A width or length that decides a layout is a named
  constant, such as `functionHeaderThreshold`.
- **Record fields are strict** (`StrictData`). A field that has to stay lazy says
  so with `~` and a comment saying why.

## Changing code safely

A refactor that is meant to keep behaviour:

1. **Pins the behaviour first** where tests do not.
2. **Says what it preserves.** That covers output, refusals and their reasons,
   which comments are placed, and the order in which parentheses are placed.
3. **Compares the old and new binaries** on a corpus, such as Kronor's
   migrations in both keyword cases and its Haskell files (`--haskell`). The
   output must be byte for byte the same, and so must the `-v` reports.
4. **Goes in one commit per idea**, so each can be reviewed and reverted alone.

## What is not enforced

Some rules are deliberately not enforced:

- limits on function or module length;
- bans on `where`, tuples, `Bool` or list comprehensions;
- a comment on every helper, or a type for every renderer;
- every HLint suggestion.

Each of these would be met by worse code.

## How the rules are checked

The checks catch only the plainest cases of their rules. `PgstyleSmallWhere`
does not see several stages in a `where` block of fewer than twelve bindings,
and `PgstyleNamedTuples` does not see positional data inside a record's fields.
Review covers the rest.

| Rule | Check |
|---|---|
| R1 | `PgstyleSmallWhere`: a where block of at most twelve bindings |
| R2 | `PgstyleNamedTuples`: no tuple of three or more, and no tuple inside a tuple, in a top-level signature |
| H1 | `PgstyleNamedFields`: no constructor applied to more than four positional arguments |
| H2 (signatures), exhaustive matches | GHC: `-Werror` for missing signatures, missing fields, incomplete patterns and incomplete record updates (`pgstyle.cabal`) |
| Layout | fourmolu (`fourmolu.yaml`) |
| Everything else | Review, with `.claude/skills/readability-review` |

The `Pgstyle*` rules are pgstyle's own [hopinion](https://github.com/NorfairKing/hopinion)
rules, in `lint/`. hopinion parses each module with GHC's parser and reads the
`.hie` files the build writes, so a rule sees the code as the compiler does, not
as text. Each rule carries its text, why it exists, and how to fix a finding.
`pgstyle-lint explain PgstyleNamedFields` prints them. hopinion's own built-in
rules are not used: they are another project's style.

A finding that is right to leave says so where it is, directly above the line:

```haskell
-- [allow:PgstyleSmallWhere] one recursive scanner whose states share the source and its tokens
```

The reason is required. An annotation that suppresses nothing fails the check,
so exceptions cannot outlive their code.

Code that broke a rule when the rules were adopted carries the reason
`pre-existing at adoption`. That is the debt, and
`grep -rn "pre-existing at adoption" src app test` lists it. Fix it when you next
change that code, and remove the annotation. Do not add new ones with that
reason.

The checks run from `scripts/check`:

- `scripts/check fast` runs fourmolu.
- `scripts/check lint` builds pgstyle and runs the rules over it.
- `scripts/check full` runs both, then every test.
- `scripts/check advice` prints HLint's suggestions without failing.

A rule moves from review to a check when review keeps missing it. A new rule is
a module in `lint/src/PgStyle/Lint/Rule/`, listed in `PgStyle.Lint.rules`. It
has to hold on pgstyle, with every existing finding fixed or annotated with its
reason.
