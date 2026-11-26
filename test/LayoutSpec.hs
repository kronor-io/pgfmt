module Main (main) where

import Control.Monad (forM, unless)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import PgFmt.Layout qualified as Layout
import PgFmt.Render (Dialect (..))
import PgFmt.Render qualified as Render
import System.Exit (exitFailure)


-- | Statements both renderers lay out: "PgFmt.Layout", over libpg_query's
-- tree, must write each exactly as "PgFmt.Render" does over
-- postgresql-syntax's.
sameAsRender :: [Text]
sameAsRender =
    [ "select a, b from t where x = 1"
    , "select (a).b, $1.f, ((a)).c from t"
    , "select a != b, a <> b, a->>'k', a -> 'k' = b, -a, +a, - 1, ~a from t"
    , "select f(a => 1, b := 2), count(*), count(distinct x), string_agg(x, ',' order by y), sum(x) filter (where y > 0) over (partition by z order by w desc nulls last) from t"
    , "select x::int, x::integer, x::int4, x::numeric(10, 2), x::text[], x::timestamp with time zone, x::double precision, cast(x as bigint), interval '1 day', date '2020-01-01', interval '1' day from t"
    , "select coalesce(a, b), greatest(a, b), least(a, 1), nullif(a, b), row(1, 2), (1, 2), array[1, 2], array[[1, 2], [3, 4]], array(select 1) from t"
    , "select exists (select 1 from u), (select max(x) from u), a in (select x from u), a not in (select x from u), a = any (select x from u), a = any(b), a <> all(b) from t"
    , "select extract(year from x), extract(epoch from x), position('a' in b), substring(x from 1 for 2), substring(x, 1, 2), trim(both 'x' from y), trim(y), now() at time zone 'utc', current_date, current_timestamp, localtime from t"
    , "select case when a then b when c then d else e end, case x when 1 then 'a' end from t"
    , "select a is null, a isnull, a is not null, a is true, a is distinct from b, a is not distinct from b, a between 1 and 2, a not between 1 and 2, a like 'x%', a not ilike 'y' from t"
    , "select * from a join b on a.id = b.id left join c using (id) cross join d natural join e inner join f on true left outer join g on g.x = a.x and g.y = 2"
    , "select * from (select 1) s, lateral (select 2) l, generate_series(1, 10) with ordinality as g(i, n), unnest(arr) u"
    , "select * from ((a join b on true) join c on true)"
    , "with x as (select 1), y (a, b) as materialized (select 1, 2) select * from x, y"
    , "with recursive r as (select 1 union all select n + 1 from r where n < 10) select * from r"
    , "select 1 union select 2 union all select 3 intersect select 4 except select 5"
    , "(select 1 order by 1) union (select 2)"
    , "select a from t order by a desc, b asc nulls first limit 10 offset 5"
    , "select a from t offset 5 limit 10"
    , "select a from t for update of t skip locked"
    , "select distinct on (a) a, b from t"
    , "select a, count(*) from t group by rollup (a, b) having count(*) > 1"
    , "insert into t as x (a, b, c, d) select 1, 2, 3, 4 on conflict on constraint t_pk do nothing"
    , "insert into t default values"
    , "update t set a = 1, b = 2 from u where t.id = u.id returning t.a as aa, t.b bb"
    , "update t set (a, b) = (1, 2) where id = 1"
    , "update t x set a = (select max(y) from u) where x.id = 1"
    , "delete from t using u where t.id = u.id returning *"
    , "select jsonb_build_object('a', 1, 'b', 2, 'c', jsonb_build_object('d', 3, 'e', 4))"
    , "select f(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, ccccccccccccccccccccccccccc(ddddddddddddddddddddddd, eeeeeeeeeeeeeeeeeeeee, fffffffffffffffffffffff))"
    , "select a from t where exists (select 1 from uuuuuuuuuuuuuuuuuuuuuu u where u.aaaaaaaaaaaaaaaaaaaaaaaa = t.aaaaaaaaaaaaaaaaaaaaaaaaaaaaa and u.b = 1)"
    , "select (case when a then 1 else 2 end)::text, (x + 1) * 2, -(x), not (a and b) from t"
    , "select \"Foo\", \"select\", t.\"Bar\", \"left\"(x, 1), left(x, 2) from \"MyTable\" t"
    , "select 007, 1.5, 100.0, 'it''s', b'0101', x'FF', true, null"
    , "select a from t where a = 1 and (b = 2 or c = 3) and d between 1 and 2 and e = 4"
    , "select x from t where (a = 1 and b = 2) and c = 3"
    , "select x from t where ((a and b) and c) and d or e"
    , "select (a or b) or c, ((a)) and b from t"
    , "select row_number() over (order by a rows between unbounded preceding and current row) from t window w as (partition by b)"
    , "select x from t tablesample bernoulli (5) repeatable (42) where a = 1"
    , "select a as \"Alias\", b \"B\", t.c as c from t"
    , "select a[1], a[1:2], b.c[3] from t"
    , "call my_proc(1, 'a')"
    , "select x collate \"C\" from t"
    , "update t set a = case when b then 1 else 2 end, c = coalesce(d, e, fffffffffffffffffffffffffffffffffffffff, gggggggggggggggggggggggggggggggggggg) where id = 1"
    , "select a from t where a in (1, -2) and b = -1"
    , "select x from t join (select a from u) s on s.a = t.a join lateral f(t.a, t.b) z on true"
    , "select * from t where not exists (select 1 from u where u.a = t.a) and t.b is not null"
    , "select pc.a from p pc, jsonb_to_recordset(pc.j) as c (fingerprint text, masked_pan text)"
    ]


-- | Statements with an expected layout: keywords in upper case, what
-- postgresql-syntax could not read, and comments.
expected :: [(Bool, Text, Text)]
expected =
    [ (True, "select a, x::int, x::text, count(*) from t where x is not null", "SELECT\n    a\n  , x::INT\n  , x::text\n  , count(*)\nFROM t\nWHERE x IS NOT NULL")
    , (True, "select name, type from t where value = 1 and y between 1 and 2", "SELECT\n    name\n  , type\nFROM t\nWHERE value = 1\n  AND y BETWEEN 1 AND 2")
    , (False, "select x::varchar(10), y::time(3) with time zone from t", "select\n    x::varchar (10)\n  , y::time (3) with time zone\nfrom t")
    , (False, "select E'a\\nb', 1.50", "select\n    E'a\\nb'\n  , 1.50")
    , -- Values rows go on one line each when every row fits, unlike
      -- "PgFmt.Render"; a row too long, or with a comment inside, puts
      -- every row's values one per line.
      (False, "values (1, 'a'), (2, 'b')", "values\n    (1, 'a'),\n    (2, 'b')")
    , (False, "insert into t (a, b) values (1, 2) on conflict (a) do update set b = excluded.b where t.b <> excluded.b returning *", "insert into t\n    (a, b)\nvalues\n    (1, 2)\non conflict (a)\ndo update set\n    b = excluded.b\nwhere t.b <> excluded.b\nreturning *")
    , (False, "values (1, 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'), (2, 'b')", "values\n    ( 1\n    , 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'\n    ),\n    ( 2\n    , 'b'\n    )")
    , (False, "values (1, -- one\n 'a'), (2, 'b')", "values\n    ( 1 -- one\n    , 'a'\n    ),\n    ( 2\n    , 'b'\n    )")
    , -- A blank line between rows is kept; one after "values" is not.
      (False, "values\n\n(1), (2),\n\n(3), -- x\n\n(4)", "values\n    (1),\n    (2),\n\n    (3), -- x\n\n    (4)")
    , -- Dollar-quoted strings are kept as written; plain ones continued over
      -- lines are joined.
      (False, "select $$it's$$, $f$ a\n b $f$, 'a'\n  'b'", "select\n    $$it's$$\n  , $f$ a\n b $f$\n  , 'ab'")
    , -- A string's lines stay as they are, however deep the layout puts it.
      (False, "with x as (select 'a\nb' as s) select s from x", "with x as (\n    select 'a\nb' as s\n)\nselect s\nfrom x")
    , -- Comments go where the layout breaks a line at their place: one on a
      -- line with code at the end of the line before the break, one on a
      -- line of its own on a line of its own after it.
      (False, "select a, -- the a\n  -- before b\n  b /* the b */\n  from t -- the t\n  /* where next */ where x = 1", "select\n    a -- the a\n    -- before b\n  , b /* the b */\nfrom t -- the t\n/* where next */\nwhere x = 1")
    , -- A list with a comment in it takes its multi-line form.
      (False, "select f(a, -- first\n  b)", "select f(\n      a -- first\n    , b\n)")
    , (False, "select x from t where a in (1, -- one\n  2) and b = 1", "select x\nfrom t\nwhere a in (\n      1 -- one\n    , 2\n)\n  and b = 1")
    , (False, "select array[1, -- one\n  2]", "select array[\n      1 -- one\n    , 2\n]")
    , (False, "values (1, 'a'), -- one\n  (2, 'b')", "values\n    (1, 'a'), -- one\n    (2, 'b')")
    , (False, "update t set a = 1, -- the a\n  b = 2 where id = 1", "update t\nset\n    a = 1 -- the a\n  , b = 2\nwhere id = 1")
    , (False, "insert into t (a, b) -- columns\n  select 1, 2 -- values\n  on conflict do nothing", "insert into t\n    (a, b) -- columns\nselect\n    1\n  , 2 -- values\non conflict\ndo nothing")
    , (False, "select a from t group by -- grouped\n  a", "select a\nfrom t\ngroup by -- grouped\n    a")
    , (False, "with x as (select 1), -- first\n  y as (select 2) select * from x, y", "with x as (\n    select 1\n), -- first\ny as (\n    select 2\n)\nselect *\nfrom x, y")
    , (True, "select 1 -- one\nunion all select 2", "SELECT 1 -- one\nUNION ALL\nSELECT 2")
    , -- A blank line before a comment is kept.
      (False, "select f(a, -- one\n\n  -- two\n  b)", "select f(\n      a -- one\n\n      -- two\n    , b\n)")
    , -- create table: one element per line, trailing commas, the source's
      -- own keywords; types with no space before their modifiers.
      (False, "CREATE TEMPORARY TABLE IF NOT EXISTS s.t (id bigint NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1 CACHE 10), price numeric (10,2) DEFAULT 0, ref text references u (id) on delete no action, PRIMARY KEY (id)) ON COMMIT DROP", "create temporary table if not exists s.t (\n    id bigint not null generated always as identity (start with 1 cache 10),\n    price numeric(10, 2) default 0,\n    ref text references u(id) on delete no action,\n    primary key (id)\n) on commit drop")
    , (True, "create table t (a int not null, constraint a_positive check (a > 0)) partition by range (a)", "CREATE TABLE t (\n    a INT NOT NULL,\n    CONSTRAINT a_positive CHECK (a > 0)\n) PARTITION BY RANGE (a)")
    , (False, "create table t (like u including defaults)", "create table t (\n    like u including defaults\n)")
    , (False, "create table t ()", "create table t ()")
    , -- A long check breaks at its top-level ands (or ors); a long foreign
      -- key before references.
      (False, "create table t (a text, b text, constraint some_check check (char_length(a) <= 100 and trim(both ' ' from a) <> '' and b is not null and b <> 'none'), constraint fk_t_long foreign key (a, b) references some_schema.other_table(a, b) on delete cascade)", "create table t (\n    a text,\n    b text,\n    constraint some_check check (\n        char_length(a) <= 100\n        and trim(both ' ' from a) <> ''\n        and b is not null\n        and b <> 'none'\n    ),\n    constraint fk_t_long foreign key (a, b)\n        references some_schema.other_table(a, b) on delete cascade\n)")
    , (False, "create table t (total int generated always as (a + b) stored, d date default current_date unique deferrable initially deferred)", "create table t (\n    total int generated always as (a + b) stored,\n    d date default current_date unique deferrable initially deferred\n)")
    , -- Comments and blank lines between columns are kept; a comment
      -- between a constraint's name and its body puts the body under it.
      (False, "create table t ( -- the table\n  a int, -- the a\n\n  -- the b\n  b int,\n  constraint b_check\n    -- b is small\n    check (b < 10)\n  -- the end\n)", "create table t ( -- the table\n    a int, -- the a\n\n    -- the b\n    b int,\n    constraint b_check\n        -- b is small\n        check (b < 10)\n    -- the end\n)")
    , -- create view and create ... as: the query laid out under the head.
      (False, "create or replace view v (x, y) as select a, b from t where c with local check option", "create or replace view v (x, y) as\nselect\n    a\n  , b\nfrom t\nwhere c\nwith local check option")
    , (True, "create materialized view m as select a from t with no data", "CREATE MATERIALIZED VIEW m AS\nSELECT a\nFROM t\nWITH NO DATA")
    , (False, "create temp table t on commit drop as select 1", "create temp table t on commit drop as\nselect 1")
    ]


-- | Statements the renderer leaves to the caller.
refused :: [Text]
refused =
    [ "create index i on t (a)"
    , "create table t (a int, exclude using gist (a with =))"
    , "create table t (a int) with (fillfactor = 70)"
    , "create table p partition of t for values from (1) to (2)"
    , "create recursive view v (n) as select 1 union all select n + 1 from v"
    , "select x from t where a similar to 'b'"
    , "select 1; select 2"
    , -- No line break where the comment is.
      "select a + -- inside\n  b from t"
    , "select 'a' -- between the parts of a string\n  'b'"
    ]


main :: IO ()
main = do
    same <- forM sameAsRender \sql -> do
        laid <- Layout.formatStatement False (TE.encodeUtf8 sql)
        let old = Render.formatStatement PlainSql sql
        case (laid, old) of
            (Right new, Right o) | new == o -> pure True
            _ -> do
                TIO.putStrLn ("FAIL (same as Render): " <> sql <> "\n  layout: " <> T.pack (show laid) <> "\n  render: " <> T.pack (show old))
                pure False
    exact <- forM expected \(upper, sql, want) -> do
        laid <- Layout.formatStatement upper (TE.encodeUtf8 sql)
        if laid == Right want
            then pure True
            else do
                TIO.putStrLn ("FAIL (expected): " <> sql <> "\n  got:  " <> T.pack (show laid) <> "\n  want: " <> T.pack (show want))
                pure False
    refusals <- forM refused \sql -> do
        laid <- Layout.formatStatement False (TE.encodeUtf8 sql)
        unless (isLeft laid) (TIO.putStrLn ("FAIL (refused): " <> sql <> "\n  got: " <> T.pack (show laid)))
        pure (isLeft laid)
    let results = same ++ exact ++ refusals
    putStrLn (show (length (filter id results)) <> " of " <> show (length results) <> " passed")
    unless (and results) exitFailure
