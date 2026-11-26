# pgfmt

A formatter for PostgreSQL files that does not change what any statement does.

It turns this:

```sql
-- Orders placed this week, with their customer.
select o.id, o.total, c.name as customer from orders o join customers c on c.id = o.customer_id where o.placed_at >= now() - interval '7 days' and o.status in ('paid', 'shipped') order by o.placed_at desc limit 50;
```

into this:

```sql
-- Orders placed this week, with their customer.
select
    o.id
  , o.total
  , c.name as customer
from orders o
join customers c
  on c.id = o.customer_id
where o.placed_at >= now() - interval '7 days'
  and o.status in ('paid', 'shipped')
order by o.placed_at desc
limit 50;
```

It reads files with PostgreSQL's own parser
([libpg_query](https://github.com/pganalyze/libpg_query)), so anything
PostgreSQL accepts, pgfmt accepts: DDL, DML, `create function` and PL/pgSQL
bodies. Before it rewrites a statement, it parses the new text again and checks
that PostgreSQL sees the same statement. If the check fails, the statement is
left as it was.

pgfmt is one of the tools Kronor uses to keep a PostgreSQL schema in sqitch, with [sqiff](https://github.com/kronor-io/sqiff), [sqint](https://github.com/kronor-io/sqint), [create-migration](https://github.com/kronor-io/sqitch-with-templates) and [psql-retry](https://github.com/kronor-io/psql-retry). [sqitchcraft](https://kronor-io.github.io/sqitchcraft/) shows how they fit together: the repository layout they assume, one change from an edited file to a deploy, and instructions for coding agents.

## Install

pgfmt is a single binary.

On Linux, each [release](https://github.com/kronor-io/pgfmt/releases) has a
static binary for `x86_64` and `aarch64` that runs on any distribution. Unpack
it into `~/.local` (or `/usr/local`) to get `bin/pgfmt` and its bash, zsh and
fish completions:

```bash
curl -fsSL "https://github.com/kronor-io/pgfmt/releases/latest/download/pgfmt-$(uname -m)-linux.tar.xz" \
  | tar -xJ -C ~/.local
```

`SHA256SUMS` on the release has the tarballs' checksums.

Elsewhere, and for development, the easiest way to get it is
[Nix](https://nixos.org/download) with flakes enabled. CI pushes every build to
the `kronor-open.cachix.org` binary cache, and the flake offers to use it:
answer yes, or pass `--accept-flake-config`, and Nix downloads pgfmt instead of
compiling it and its Haskell dependencies.

Try it without installing:

```bash
nix run --accept-flake-config github:kronor-io/pgfmt -- --help
```

Install it into your profile:

```bash
nix profile install --accept-flake-config github:kronor-io/pgfmt
```

Or add it to a flake's dev shell:

```nix
{
  inputs.pgfmt.url = "github:kronor-io/pgfmt";

  outputs = { nixpkgs, pgfmt, ... }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [ pgfmt.packages.x86_64-linux.default ];
    };
  };
}
```

It builds for `x86_64-linux`, `aarch64-linux` and `aarch64-darwin`. To build
without Nix, see [Building from source](#building-from-source).

## Usage

### Format files

Name the files. They are rewritten in place, and files that are already
formatted are not touched:

```bash
pgfmt migrations/*.sql
```

With no files, or `-`, pgfmt reads stdin and writes stdout, which is what
editors want:

```bash
pgfmt < query.sql
```

Each statement is formatted on its own. Comments, blank lines and anything else
between statements stay byte for byte, so a file keeps its shape:

```sql
-- Orders placed this week, with their customer.
select o.id, o.total from orders o where o.status = 'paid';

-- Refunds need a positive amount.
CREATE TABLE refunds (id bigint generated always as identity primary key, order_id bigint not null references orders(id), amount numeric(12,2) not null check (amount > 0), created_at timestamptz not null default now());
```

```sql
-- Orders placed this week, with their customer.
select
    o.id
  , o.total
from orders o
where o.status = 'paid';

-- Refunds need a positive amount.
create table refunds (
    id bigint generated always as identity primary key,
    order_id bigint not null references orders (id),
    amount numeric(12, 2) not null check (amount > 0),
    created_at timestamptz not null default now()
);
```

### Check in CI

`--check` changes nothing. It names the files that are not formatted and
exits 1:

```console
$ pgfmt --check migrations/*.sql
migrations/0042-refunds.sql: not formatted
$ echo $?
1
```

In GitHub Actions:

```yaml
- uses: cachix/install-nix-action@v31
- run: nix run --accept-flake-config github:kronor-io/pgfmt -- --check $(git ls-files '*.sql')
```

As a [pre-commit](https://pre-commit.com) hook, with pgfmt on your `PATH`:

```yaml
repos:
  - repo: local
    hooks:
      - id: pgfmt
        name: pgfmt
        entry: pgfmt
        language: system
        files: \.sql$
```

### Exit codes

| Code | Meaning |
| ---- | ------- |
| 0    | Done; with `--check`, every file is formatted. |
| 1    | `--check` only: at least one file is not formatted. |
| 2    | A file is not valid SQL, or a statement failed verification (a formatter bug; that statement is left as it was). |

A file pgfmt cannot parse is never rewritten:

```console
$ echo 'selec 1;' | pgfmt
<stdin>: not valid SQL at byte 1: syntax error at or near "selec"
```

### See what was skipped

Some statements are left as they were on purpose, for example when the
renderer cannot keep a comment inside them. `-v` says which ones, and why:

```console
$ pgfmt -v --backend syntax queries.sql
queries.sql:2: kept: comments inside the statement
```

## Choosing a style

### Keyword case

`--keyword-case lower` (the default) or `--keyword-case upper`. Every
statement pgfmt formats is spelled one way. Identifiers are never changed,
including the ones that look like keywords:

```console
$ echo "SELECT name, type FROM users WHERE value > 3;" | pgfmt --keyword-case upper
SELECT
    name
  , type
FROM users
WHERE value > 3;
```

### Backends

Two backends lay statements out, and they have different styles:

- `syntax`: pgfmt's own renderer. It puts commas at the start of lines, and
  it handles `select`, `insert`, `update`, `delete` and `values` that have no
  comments inside them.
- `libpg-query`: libpg_query's pretty-printer. It puts commas at the end of
  lines, and it handles anything PostgreSQL parses, keeping the comments.

The default, `--backend auto`, uses `syntax` where it can and `libpg-query`
for the rest. Pick one backend to get a single style across the whole file:

```console
$ echo "select o.id, o.total, c.name from orders o join customers c on c.id = o.customer_id where o.status = 'paid';" | pgfmt --backend libpg-query
select o.id, o.total, c.name
from
    orders o
    join customers c on c.id = o.customer_id
where o.status = 'paid';
```

The `libpg-query` backend also takes `--indent N` (spaces per level,
default 4) and `--width N` (the line length it wraps lists at, default 80).

DDL always goes to `libpg-query`:

```sql
alter table orders
    add column note text,
    add constraint note_len check (length(note) < 500);
create index concurrently orders_status_idx
on orders using btree (status)
where status <> 'paid';
```

## PL/pgSQL

To PostgreSQL, a function body is just a string. pgfmt also parses
PL/pgSQL bodies, and it formats the SQL statements inside them, indented to
where each one starts:

```sql
create function expire_pending_orders(cutoff interval) returns integer
language plpgsql as $$
declare
    expired integer;
begin
    update orders set status = 'expired', updated_at = now() where status = 'pending' and placed_at < now() - cutoff;
    get diagnostics expired = row_count;
    return expired;
end;
$$;
```

```sql
create function expire_pending_orders(cutoff interval) returns int language plpgsql as $$
declare
    expired integer;
begin
    update orders
    set
        status = 'expired'
      , updated_at = now()
    where status = 'pending'
      and placed_at < now() - cutoff;
    get diagnostics expired = row_count;
    return expired;
end;
$$;
```

The statements it formats are `insert`, `update`, `delete` and `select` run
for their effect, and `return query`. Everything else is left as written: the
PL/pgSQL around them (`declare`, `if`, loops, assignments) and statements with
`into`. The new body is only used if it parses to the same PL/pgSQL as the old
one.

pgfmt skips functions that libpg_query cannot compile. It does not look up
types outside `pg_catalog` and `public`, so a function that declares a
variable of type `myschema.thing` is left alone.

## How it stays safe

1. **Split.** libpg_query parses the whole file and gives each statement's
   byte range. Everything between statements is copied through unchanged.
2. **Format.** A backend lays out each statement.
3. **Verify.** libpg_query parses the new text. It must be one statement with
   the same parse tree (compared by its compact deparse) and the same number
   of comments as the original. This check does not depend on either backend.
   Finally, the whole new file must parse to the same statements as the old
   one.

The check compares meaning, not spelling. libpg_query's backend writes some
things differently, and those changes pass: `integer` becomes `int`, an index
gets `using btree`, and `interval '7 days'` can become `'7 days'::interval`.

## Limits

- Files with psql meta-commands (`\set`, `\i`, `\gexec`) are not SQL, and
  pgfmt rejects them.
- Files that set `standard_conforming_strings = off` are left alone.
- `--backend auto` can mix the two styles in one file. Choose a backend if
  that matters to you.
- libpg_query moves a comment that it cannot attach to a node to the end of
  the statement: `select a -- x` followed by `from t` comes out as `select a`,
  `from t`, `-- x`.
- The `syntax` renderer fails its own check on some statements with
  `$$`-quoted strings that contain comments. Those statements are left as they
  are and pgfmt exits 2.

## hasql-th

`--dialect hasql-th` lets the `syntax` backend read the extensions that
[hasql-th](https://github.com/kronor-io/hasql-th) adds to SQL in its
quasiquotes (`$1 :: int8?`, `$Record {..}`, `$1.$field`). libpg_query cannot
read those, so a file using them is not valid SQL to pgfmt. To format a
quasiquote's SQL, use the library's `PgFmt.Render.formatSql`.

## Library

pgfmt is also a Haskell library:

- `PgFmt.File.formatFile`: a whole file, with a report on each statement.
- `PgFmt.Render.formatStatement`: one DML statement with the `syntax` layout.
- `PgFmt.Render.formatSql`: the SQL of one hasql-th quasiquote.

## Building from source

pgfmt links libpg_query 18, the major version that the `libpg-query`
package's protobuf definitions were generated from.

With Nix:

```bash
nix build            # result/bin/pgfmt
nix flake check      # the test suites
```

The flake applies one patch to libpg_query,
[`nix/libpg_query-set-values-as-strings.patch`](nix/libpg_query-set-values-as-strings.patch).
Without it, libpg_query writes a `set` value that is not a plain lowercase
word as a quoted identifier: `set lock_timeout to '50ms'` comes out as
`set lock_timeout to "50ms"`. PostgreSQL reads both the same way, but the
patched library keeps the string.

The release binaries come from `scripts/build-static`, which builds the
patched libpg_query and pgfmt in a ghc-musl container with Docker and writes
`dist-static/pgfmt-<arch>-linux.tar.xz`.

With cabal (GHC 9.12), build libpg_query 18 with that patch
(`patch -p1 < /path/to/pgfmt/nix/libpg_query-set-values-as-strings.patch` in
its source tree) and point cabal at it in `cabal.project.local`:

```cabal
package libpg-query
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
package pgfmt
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
```

```bash
cabal build exe:pgfmt
cabal test
```
