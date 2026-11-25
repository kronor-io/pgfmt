# pgfmt

Format PostgreSQL files, statement by statement, without changing what any
statement means.

```
pgfmt [--check] [--backend auto|syntax|libpg-query] [--keyword-case lower|upper]
      [--dialect plain|hasql-th] [FILE...]
```

With no files, or `-`, it reads stdin and writes stdout. `--check` changes
nothing and exits 1 if a file is not formatted. It exits 2 when a file is not
valid SQL, or when a backend produced SQL that means something else (a
formatter bug; that statement is left as it was).

## How it works

1. **Split.** libpg_query, PostgreSQL's own parser as a library, parses the
   file and gives each top-level statement's byte range. Everything between
   statements (whitespace, comments, semicolons) is kept byte for byte.
2. **Format.** Each statement is laid out by one of two backends:
   - `syntax`: pgfmt's own renderer over
     [postgresql-syntax](https://github.com/kronor-io/postgresql-syntax).
     DML only (SELECT, INSERT, UPDATE, DELETE, VALUES), and only statements
     without comments inside them, since postgresql-syntax drops comments.
   - `libpg-query`: libpg_query's pretty-printer. Any statement PostgreSQL
     parses, DDL and function bodies included, with comments put back.
   - `auto` (the default): `syntax` where it applies, `libpg-query` for the
     rest.
3. **Verify.** A statement is only rewritten if libpg_query parses the result
   as one statement with the same normal form (its compact deparse) and the
   same number of comments as the original. This check is independent of
   either backend: the `syntax` backend also checks its own output against
   postgresql-syntax's AST.

## Keyword case

`--keyword-case lower` (the default) or `upper` spells the keywords of every
formatted statement one way, whichever backend laid it out; identifiers are
never touched, including the ones spelled like keywords (`name`, `type`,
`value`). libpg_query writes keywords in upper case and identifiers in lower
case, which tells them apart in its output. For the `syntax` backend, pgfmt
renders the statement a second time with every identifier's letters replaced
by `q`: same length, same layout, so the tokens that differ between the two
renderings are the identifiers. Statements left as they were keep their
original spelling.

## Dialects

`--dialect hasql-th` lets the `syntax` backend parse the extensions
[hasql-th](https://github.com/kronor-io/hasql-th) adds to SQL in its
quasiquotes (`$1 :: int8?`, `$Record {..}`, `$1.$field`). libpg_query cannot
read those, so files using them are not valid SQL to pgfmt; the library's
`formatSql` formats one quasiquote's SQL instead, which is what Kronor's
`hasql-th-format` calls.

## Library

- `PgFmt.File.formatFile`: a whole file.
- `PgFmt.Render.formatStatement`: one DML statement with pgfmt's layout.
- `PgFmt.Render.formatSql`: the SQL of one hasql-th quasiquote.

## Building

pgfmt links libpg_query 18 (the major the `libpg-query` package's protobuf
was generated from).

```bash
nix build            # bin/pgfmt
nix flake check      # the test suites
```

With cabal, point it at a libpg_query 18 build in `cabal.project.local`:

```cabal
package libpg-query
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
package pgfmt
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
```

## Open questions

- The two backends share keyword case, not layout: the `syntax` backend puts
  commas at the start of lines, libpg_query at the end.
- libpg_query's deparse normalises some spellings (for example, it may write
  a cast differently). Those changes pass verification, since the normal form
  is the same, but may not be wanted.
- libpg_query places a comment it cannot attach to a node at the end of the
  statement (`SELECT a -- x\nFROM t` becomes `SELECT a\nFROM t\n-- x`).
  pgfmt ends the line after it, so it cannot swallow the semicolon, but the
  comment still moves.
- The `syntax` renderer fails its own AST check on statements with
  `$$`-quoted strings that contain comments (three of Kronor's pg_timetable
  migrations). Those statements are left as they are and pgfmt exits 2.
- Release tarballs (static, musl) are not set up yet.
