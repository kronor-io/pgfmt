# pgstyle

A formatter for PostgreSQL files that does not change what any statement does.
Up to 0.1.1.0 it was called pgfmt.

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
PostgreSQL accepts, pgstyle accepts: DDL, DML, `create function` and PL/pgSQL
bodies. Before it rewrites a statement, it parses the new text again and checks
that PostgreSQL sees the same statement. If the check fails, the statement is
left as it was.

It also formats the SQL in a Haskell file's
[hasql-th quasiquotes](#haskell-hasql-th-quasiquotes).

pgstyle is one of the tools Kronor uses to keep a PostgreSQL schema in sqitch, with [sqiff](https://github.com/kronor-io/sqiff), [sqint](https://github.com/kronor-io/sqint), [create-migration](https://github.com/kronor-io/sqitch-with-templates) and [psql-retry](https://github.com/kronor-io/psql-retry). [sqitchcraft](https://kronor-io.github.io/sqitchcraft/) shows how they fit together: the repository layout they assume, one change from an edited file to a deploy, and instructions for coding agents.

## Install

pgstyle is a single binary.

On Linux, each [release](https://github.com/kronor-io/pgstyle/releases) has a
static binary for `x86_64` and `aarch64` that runs on any distribution. Unpack
it into `~/.local` (or `/usr/local`) to get `bin/pgstyle` and its bash, zsh and
fish completions:

```bash
curl -fsSL "https://github.com/kronor-io/pgstyle/releases/latest/download/pgstyle-$(uname -m)-linux.tar.xz" \
  | tar -xJ -C ~/.local
```

`SHA256SUMS` on the release has the tarballs' checksums.

The same binary is an image, `ghcr.io/kronor-io/pgstyle`, for `linux/amd64` and
`linux/arm64`, tagged with each version and `latest`. It holds pgstyle alone, no
shell, and runs it in `/work`. Mount your files there and run it as yourself,
so the files it rewrites stay yours:

```bash
docker run --rm -v "$PWD:/work" -u "$(id -u):$(id -g)" ghcr.io/kronor-io/pgstyle --check migrations/*.sql
```

Elsewhere, and for development, the easiest way to get it is
[Nix](https://nixos.org/download) with flakes enabled. CI pushes every build to
the `kronor-open.cachix.org` binary cache, and the flake offers to use it:
answer yes, or pass `--accept-flake-config`, and Nix downloads pgstyle instead
of compiling it and its Haskell dependencies.

Try it without installing:

```bash
nix run --accept-flake-config github:kronor-io/pgstyle -- --help
```

Install it into your profile:

```bash
nix profile install --accept-flake-config github:kronor-io/pgstyle
```

Or add it to a flake's dev shell:

```nix
{
  inputs.pgstyle.url = "github:kronor-io/pgstyle";

  outputs = { nixpkgs, pgstyle, ... }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
      packages = [ pgstyle.packages.x86_64-linux.default ];
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
pgstyle migrations/*.sql
```

With no files, or `-`, pgstyle reads stdin and writes stdout, which is what
editors want:

```bash
pgstyle < query.sql
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
    order_id bigint not null references orders(id),
    amount numeric(12, 2) not null check (amount > 0),
    created_at timestamptz not null default now()
);
```

### Check in CI

`--check` changes nothing. It names the files that are not formatted and
exits 1:

```console
$ pgstyle --check migrations/*.sql
migrations/0042-refunds.sql: not formatted
$ echo $?
1
```

In GitHub Actions:

```yaml
- uses: cachix/install-nix-action@v31
- run: nix run --accept-flake-config github:kronor-io/pgstyle -- --check $(git ls-files '*.sql')
```

As a [pre-commit](https://pre-commit.com) hook, with pgstyle on your `PATH`:

```yaml
repos:
  - repo: local
    hooks:
      - id: pgstyle
        name: pgstyle
        entry: pgstyle
        language: system
        files: \.sql$
```

### Exit codes

| Code | Meaning |
| ---- | ------- |
| 0    | Done; with `--check`, every file is formatted. |
| 1    | `--check` only: at least one file is not formatted. |
| 2    | A file is not valid SQL, or a statement failed verification (a formatter bug; that statement is left as it was). |

A file pgstyle cannot parse is never rewritten:

```console
$ echo 'selec 1;' | pgstyle
<stdin>: not valid SQL at byte 1: syntax error at or near "selec"
```

### See what was skipped

Some statements are left as they were on purpose, for example when the
renderer has no place for a comment inside them. `-v` says which ones, and
why:

```console
$ pgstyle -v --backend syntax queries.sql
queries.sql:2: kept: pgstyle does not lay it out: a comment pgstyle cannot place
```

## Choosing a style

### Keyword case

`--keyword-case lower` (the default) or `--keyword-case upper`. Every
statement pgstyle formats is spelled one way. Identifiers are never changed,
including the ones that look like keywords:

```console
$ echo "SELECT name, type FROM users WHERE value > 3;" | pgstyle --keyword-case upper
SELECT
    name
  , type
FROM users
WHERE value > 3;
```

### Backends

Two backends lay statements out, and they have different styles:

- `syntax`: pgstyle's own renderer. It handles `select`, `insert`, `update`,
  `delete` and `values`, with commas at the start of lines, and `create
  table`, `create view`, `create index`, `alter table` (see below) and
  `create function` (see [PL/pgSQL](#plpgsql)). It lays
  statements out from PostgreSQL's own parse tree (libpg_query's), keeping
  what the tree forgets from the source: redundant parentheses, `!=`, `as`
  before an alias, `inner join`, how a type is spelled, and comments. A
  comment goes where the layout breaks a line at its place: one that shares a
  line with code ends the line before the break, one on a line of its own
  stays on a line of its own. A list with a comment in it (select targets,
  arguments, `in (...)`, `values` rows, ...) puts one item per line:

  ```sql
  select
      id -- the key
      -- what the customer sees
    , display_name
  from customers
  where status in (
        'active' -- paying
      , 'trial'
  );
  ```

  A statement with something it does not handle, such as a comment with no
  line break at its place (`a + -- note` and `b` on the next line), is left to
  `libpg-query` (or, with `--backend syntax`, as it is), never guessed at.
  (The SQL of Haskell quasiquotes, which libpg_query cannot parse, is laid out
  the same way from postgresql-syntax's tree, but not with comments inside.)
- `libpg-query`: libpg_query's pretty-printer. It puts commas at the end of
  lines, and it handles anything PostgreSQL parses, keeping the comments.

The default, `--backend auto`, uses `syntax` where it can and `libpg-query`
for the rest. Pick one backend to get a single style across the whole file:

```console
$ echo "select o.id, o.total, c.name from orders o join customers c on c.id = o.customer_id where o.status = 'paid';" | pgstyle --backend libpg-query
select o.id, o.total, c.name
from
    orders o
    join customers c on c.id = o.customer_id
where o.status = 'paid';
```

The `libpg-query` backend also takes `--indent N` (spaces per level,
default 4) and `--width N` (the line length it wraps lists at, default 80).

`create table` goes to `syntax` too, one column or constraint per line with
the comma at the end, as tables are usually written. It keeps the source's
own words (`temp` or `temporary`, an explicit `on delete no action`), blank
lines and comments between columns, and breaks a long `check` at its
top-level `and`s and a long foreign key before `references`:

```sql
create table refunds (
    id bigint generated always as identity primary key,
    order_id bigint not null references orders(id),

    -- Never more than the order's total.
    amount numeric(12, 2) not null,
    constraint fk_refund_order_line foreign key (order_id, line_id)
        references order_lines(order_id, id) on delete cascade,
    constraint amount_positive check (amount > 0)
);
```

`create view`, `create materialized view` and `create table ... as` put the
query on the lines after their head, laid out as a statement of its own:

```sql
create view paid_orders as
select
    o.id
  , o.total
from orders o
where o.status = 'paid';
```

`create index` puts `on` and `where` on lines of their own, and
`alter table` one action per line with the comma at the end. A long
condition, in an index's `where` or a `check`, breaks at its top-level
`and`s or `or`s:

```sql
alter table orders
    add column note text,
    add constraint note_len check (length(note) < 500);
create index concurrently orders_status_idx
    on orders (status)
    where status <> 'paid';
```

`create trigger` puts each clause on a line of its own, at the statement's
column. `grant` and `revoke` stay on one line if it fits in 80 characters;
otherwise `on` and `to` (or `from`) go on lines of their own, indented:

```sql
create trigger orders_touch
before update
on orders
for each row
when (new.status <> old.status)
execute function touch_updated_at();
grant select on orders to reporting;
grant select, insert, update
    on table orders, order_lines, refunds
    to order_service, reporting;
```

Any other DDL goes to `libpg-query`, as does a statement with something
`syntax` does not handle (`exclude`, `tablespace`, `attach partition`, ...):

```sql
create sequence order_numbers start 1000 cache 10;
comment on table orders is 'One row per checkout.';
```

## PL/pgSQL

To PostgreSQL, a function body is just a string. pgstyle also parses
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
create function expire_pending_orders(cutoff interval)
    returns integer
    language plpgsql as
$$
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

The function's head is laid out too: its arguments on the first line, or
one per line if they do not fit, each clause before the body on a line of
its own with `as` ending the last, and the body on the next line, its
opening quote lined up with its closing one. The clauses after the body stay
on its last line. A SQL-standard body
(`begin atomic ... end`) has its statements laid out like any others.

The statements it formats are `insert`, `update`, `delete` and `select` run
for their effect, and `return query`. Everything else is left as written: the
PL/pgSQL around them (`declare`, `if`, loops, assignments) and statements with
`into`. The new body is only used if it parses to the same PL/pgSQL as the old
one.

pgstyle skips functions that libpg_query cannot compile. It does not look up
types outside `pg_catalog` and `public`, so a function that declares a
variable of type `myschema.thing` is left alone.

## How it stays safe

1. **Split.** libpg_query parses the whole file and gives each statement's
   byte range. Everything between statements is copied through unchanged.
2. **Format.** A backend lays out each statement.
3. **Verify.** libpg_query parses the new text. It must be one statement with
   the same parse tree, source positions aside, and the same number of
   comments as the original. The trees themselves are compared, not SQL
   printed from them, so two trees that print alike still count as different.
   This check does not depend on either backend. Finally, the whole new file
   must parse to the same statements as the old one.

   It does catch backend bugs. On Kronor's migrations, libpg_query's
   pretty-printer drops the quotes from function parameter names
   (`"startDate" date` comes out as `startDate date`, which PostgreSQL reads as
   `startdate`); pgstyle refuses those statements, leaves them as they are, and
   exits 2. One limit: libpg_query 18 leaves empty strings out of its trees, so
   `comment on t is ''` and `comment on t is null` look the same to it.
   PostgreSQL treats them the same too.

The check compares meaning, not spelling. libpg_query's backend writes some
things differently, and those changes pass: `integer` becomes `int`, an index
gets `using btree`, and `interval '7 days'` can become `'7 days'::interval`.

## Limits

- Files with psql meta-commands (`\set`, `\i`, `\gexec`) are not SQL, and
  pgstyle rejects them.
- Files that set `standard_conforming_strings = off` are left alone.
- `--backend auto` can mix the two styles in one file. Choose a backend if
  that matters to you.
- libpg_query moves a comment that it cannot attach to a node to the end of
  the statement: `select a -- x` followed by `from t` comes out as `select a`,
  `from t`, `-- x`.
- In Haskell quasiquotes, the `syntax` renderer fails its own check on some
  statements with `$$`-quoted strings that contain comments. Those statements
  are left as they are and pgstyle exits 2.

## Haskell: hasql-th quasiquotes

For `.hs` files, pgstyle formats the SQL in
[hasql-th](https://github.com/nikita-volkov/hasql-th) quasiquotes and leaves
the Haskell alone, so it sits next to fourmolu or ormolu, which leave
quasiquotes alone:

```bash
pgstyle src/**/*.hs
```

```haskell
userEmails :: Statement Int64 (Vector (Text, Maybe Text))
userEmails =
    [vectorStatement|
      select u.name :: text, e.address :: text? from users u left join emails e on e.user_id = u.id where u.org_id = $1 :: int8 order by u.name
    |]
```

```haskell
userEmails :: Statement Int64 (Vector (Text, Maybe Text))
userEmails =
    [vectorStatement|
        select
            u.name::text
          , e.address::text?
        from users u
        left join emails e
          on e.user_id = u.id
        where u.org_id = $1::int8
        order by u.name
    |]
```

The SQL goes one indent past the opening `[`, and the closing `|]` lines up
under it. The `syntax` backend lays it out, in lower case, with hasql-th's
extensions such as nullability markers (`text?`); `--backend` and
`--keyword-case` apply to SQL files only. A quasiquote's SQL is only replaced
if it parses back to the same postgresql-syntax AST, and the file must still
have the same quasiquotes and the same text around them.

- **Which quasiquotes:** hasql-th's `singletonStatement`, `maybeStatement`,
  `vectorStatement`, `foldStatement`, `resultlessStatement`,
  `rowsAffectedStatement`, and `uncheckedSql`, qualified or not
  (`[TH.singletonStatement|`). `--quoter NAME` adds another one, for your own
  aliases.
- **What is left alone:** single-line quasiquotes, and SQL postgresql-syntax
  does not parse (`-v` lists them), such as DDL in `uncheckedSql`.
- **Finding them:** pgstyle skips comments, string and character literals as GHC
  does, so a `[singletonStatement|` in a string is not a quasiquote. As in GHC
  with `QuasiQuotes` on, `[name|` with no space before the bar opens one.
- **stdin:** `--haskell` reads it as Haskell: `pgstyle --haskell < Foo.hs`.

`--dialect hasql-th` lets the `syntax` backend read the same extensions in a
SQL file, but libpg_query cannot read them, so such a file is not valid SQL to
pgstyle. The library's `PgStyle.Render.formatSql` formats one quasiquote's SQL.

### Kronor's extensions

Kronor's [fork of hasql-th](https://github.com/kronor-io/hasql-th) adds
Haskell targets (`$Record {field = a :: int8}`, `$f (a :: int8, b :: text)`)
and param fields (`$1.$field`). They need Kronor's fork of
postgresql-syntax, so they are behind the `kronor-syntax` cabal flag, off by
default. The flake builds that variant as `pgstyle-kronor`:

```bash
nix run --accept-flake-config github:kronor-io/pgstyle#pgstyle-kronor -- --help
```

With cabal, `cabal.project.kronor` adds Kronor's package repository and turns
the flag on:

```bash
cabal build --project-file=cabal.project.kronor exe:pgstyle
```

## Library

pgstyle is also a Haskell library:

- `PgStyle.File.formatFile`: a whole file, with a report on each statement.
- `PgStyle.Haskell.formatHaskell`: the hasql-th quasiquotes of a Haskell file,
  with a report on each.
- `PgStyle.Render.formatStatement`: one DML statement with the `syntax` layout.
- `PgStyle.Render.formatSql`: the SQL of one hasql-th quasiquote.

## Building from source

pgstyle links libpg_query 18, the major version that the `libpg-query`
package's protobuf definitions were generated from.

With Nix:

```bash
nix build                   # result/bin/pgstyle
nix build .#pgstyle-kronor    # with the kronor-syntax flag
nix flake check             # the test suites, both variants
```

The flake applies one patch to libpg_query,
[`nix/libpg_query-set-values-as-strings.patch`](nix/libpg_query-set-values-as-strings.patch).
Without it, libpg_query writes a `set` value that is not a plain lowercase
word as a quoted identifier: `set lock_timeout to '50ms'` comes out as
`set lock_timeout to "50ms"`. PostgreSQL reads both the same way, but the
patched library keeps the string.

The release binaries come from `scripts/build-static`, which builds the
patched libpg_query and pgstyle in a ghc-musl container with Docker and writes
`dist-static/pgstyle-<arch>-linux.tar.xz`. The image is the `Dockerfile` around
those binaries.

With cabal (GHC 9.12), everything else comes from Hackage. Build libpg_query
18 with that patch
(`patch -p1 < /path/to/pgstyle/nix/libpg_query-set-values-as-strings.patch` in
its source tree) and point cabal at it in `cabal.project.local` (for
`cabal.project.kronor`, `cabal.project.kronor.local`):

```cabal
package libpg-query
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
package pgstyle
  extra-include-dirs: /path/to/libpg_query/include
  extra-lib-dirs: /path/to/libpg_query/lib
```

```bash
cabal build exe:pgstyle
cabal test
```

## Contributing

`docs/readability.md` says how pgstyle's code is meant to read. Inside
`nix develop`, `scripts/check` runs the checks that CI runs:

```bash
scripts/check fast    # fourmolu
scripts/check lint    # pgstyle's hopinion rules, in lint/
scripts/check full    # both, then every test
```
