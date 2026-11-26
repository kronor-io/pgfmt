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
    , "values (1, 'a'), (2, 'b')"
    , "insert into t (a, b) values (1, 2) on conflict (a) do update set b = excluded.b where t.b <> excluded.b returning *"
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
    , "select 007, 1.5, 100.0, 'it''s', $$dollar$$, b'0101', x'FF', true, null"
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


-- | Statements with an expected layout: keywords in upper case, and what
-- postgresql-syntax could not read.
expected :: [(Bool, Text, Text)]
expected =
    [ (True, "select a, x::int, x::text, count(*) from t where x is not null", "SELECT\n    a\n  , x::INT\n  , x::text\n  , count(*)\nFROM t\nWHERE x IS NOT NULL")
    , (True, "select name, type from t where value = 1 and y between 1 and 2", "SELECT\n    name\n  , type\nFROM t\nWHERE value = 1\n  AND y BETWEEN 1 AND 2")
    , (False, "select x::varchar(10), y::time(3) with time zone from t", "select\n    x::varchar (10)\n  , y::time (3) with time zone\nfrom t")
    , (False, "select E'a\\nb', 1.50", "select\n    E'a\\nb'\n  , 1.50")
    , -- A string's lines stay as they are, however deep the layout puts it.
      (False, "with x as (select 'a\nb' as s) select s from x", "with x as (\n    select 'a\nb' as s\n)\nselect s\nfrom x")
    ]


-- | Statements the renderer leaves to the caller.
refused :: [Text]
refused =
    [ "create table t (a int)"
    , "select x from t where a similar to 'b'"
    , "select 1; select 2"
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
