# Changelog

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
