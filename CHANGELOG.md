# Changelog

## Unreleased

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
