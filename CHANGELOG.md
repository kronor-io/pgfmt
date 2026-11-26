# Changelog

## Unreleased

* The `syntax` backend lays out plain SQL statements with comments inside
  them, instead of leaving them as they are (or, with `--backend auto`, to
  `libpg-query`). Each comment goes where the layout breaks a line at its
  place in the source: one sharing a line with code ends the line before the
  break, one on a line of its own stays on one. A list with a comment in it
  (select targets, `set` assignments, call arguments, `in (...)`, `array[...]`,
  `values` rows and their items, insert columns, `group by`, `and`ed
  conditions) puts one item per line. A comment with no break at its place
  (inside an expression, or between the parts of a string continued over
  lines) still leaves the statement as it is. On Kronor's migrations, 177 of
  the 179 DML statements with comments inside are now laid out, along with
  statements with comments in PL/pgSQL bodies; statements without comments
  come out byte for byte the same. The hasql-th dialect still leaves
  statements with comments alone.
* A blank line before a comment on a line of its own is kept, so separate
  comments stay apart.
* Dollar-quoted strings (`$$...$$`, `$body$...$body$`) are kept as written
  instead of being rewritten as `'...'`, which doubled every quote in a
  multi-line job body. On Kronor's migrations these two changes touch 120
  files (mostly `timetable.add_job(...)` calls); nothing else changes.

## 0.1.1.0

* The `syntax` backend lays out plain SQL from libpg_query's parse tree
  (PostgreSQL 18's own parser) instead of postgresql-syntax's, in the same
  layout. On Kronor's migrations, every one of the 2,020 statements it formatted
  before (PL/pgSQL bodies included) comes out byte for byte the same, with
  `--keyword-case lower` and `upper`. It now also lays out DML that
  postgresql-syntax could not parse (`E'...'` strings, `varchar(n)` casts,
  column definition lists, `tablesample`, ...): 96 more statements there.
  Multi-line string literals are no longer re-indented, and numbers are kept
  as written (`1.50`), so statements the old renderer refused as changed now
  format. Statements it does not handle are still left to `libpg-query`. The
  hasql-th dialect still uses postgresql-syntax.
* The safety check compares each statement's parse tree, source positions
  aside, instead of SQL deparsed from it: two different trees the deparser
  prints alike no longer pass. On Kronor's 4,414 migrations the output is
  byte for byte the same as before. Error messages now quote the statement as
  written and as formatted, rather than the deparser's rendering of each.
* Formats the SQL in Haskell files' hasql-th quasiquotes: files ending in
  `.hs`, or stdin with `--haskell`. Comments, strings and character literals
  are skipped as GHC does; each quasiquote's SQL is replaced only if it parses
  back to the same postgresql-syntax AST, and the file must keep the same
  quasiquotes and the same text around them. `--quoter NAME` adds a quoter to
  hasql-th's. On Kronor's code it matches `hasql-th-format`, and also formats
  qualified quoters (`[TH.singletonStatement|`), which that tool skips.
* Builds against Hackage's postgresql-syntax. The hasql-th dialect is upstream
  hasql-th's; the `kronor-syntax` flag (`cabal.project.kronor`, or the flake's
  `pgfmt-kronor`) adds the Haskell targets and param fields of Kronor's fork.
* Builds with the same haskell.nix and nixpkgs as sqiff and sqint, so they
  share one GHC 9.12.4 in the kronor-open cache.

## 0.1.0.1

* Static Linux binaries for x86_64 and aarch64 on each GitHub release, with
  bash, zsh and fish completions. They are built against musl and run on any
  Linux distribution:

      curl -fsSL "https://github.com/kronor-io/pgfmt/releases/latest/download/pgfmt-$(uname -m)-linux.tar.xz" | tar -xJ -C ~/.local

## 0.1.0.0

First release.

* Formats PostgreSQL files statement by statement, and rewrites a statement
  only when libpg_query parses the new text to the same statement, with the
  same number of comments. Text between statements is copied byte for byte.
* Two backends: pgfmt's own renderer for DML, with leading commas, and
  libpg_query's pretty-printer for everything PostgreSQL parses, including all
  DDL. `--backend auto`, the default, uses the first where it can.
* `--check` for CI and pre-commit: names the files that are not formatted and
  exits 1, changing nothing.
* `--keyword-case lower|upper`, applied to every statement in the file,
  whichever backend laid it out. Identifiers are never changed.
* Formats the SQL statements inside PL/pgSQL function bodies, and keeps the new
  body only if it parses to the same PL/pgSQL.
* `SET` values keep their single quotes: `set lock_timeout to '50ms'` stays as
  it is. libpg_query 18.0.0 writes such values as quoted identifiers
  (`"50ms"`); the flake builds it with
  `nix/libpg_query-set-values-as-strings.patch`.
* `--dialect hasql-th` lets the `syntax` backend read hasql-th's quasiquote
  extensions, and the library exposes `PgFmt.Render.formatSql` for them.
* Builds with Nix for `x86_64-linux`, `aarch64-linux` and `aarch64-darwin`;
  binaries come from the kronor-open cache.
