module Main (main) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PgFmt
import System.Exit (exitFailure)


data Case = Case
    { name :: Text
    , backend :: Backend
    , keywordCase :: KeywordCase
    , input :: Text
    , expected :: Expected
    }


data Expected
    = -- | This output, with no statement failed.
      Output Text
    | -- | Unchanged, each statement kept for this reason.
      Unchanged Text
    | -- | Not valid SQL.
      Invalid


cases :: [Case]
cases =
    [ Case
        { name = "syntax: DML is laid out, the text between statements kept"
        , backend = Syntax
        , keywordCase = Lower
        , input = "-- header\n\nSELECT a, b FROM t WHERE x = 1;\n\n/* between */\nselect 1 ;\n"
        , expected = Output "-- header\n\nselect\n    a\n  , b\nfrom t\nwhere x = 1;\n\n/* between */\nselect 1 ;\n"
        }
    , Case
        { name = "syntax: DDL is left alone"
        , backend = Syntax
        , keywordCase = Lower
        , input = "CREATE TABLE t (a int, b text);\n"
        , expected = Unchanged "postgresql-syntax does not parse it"
        }
    , Case
        { name = "syntax: a comment inside a statement leaves it alone"
        , backend = Syntax
        , keywordCase = Lower
        , input = "SELECT a -- the a\nFROM t;\n"
        , expected = Unchanged "comments inside the statement"
        }
    , Case
        { name = "libpg-query: DDL is pretty-printed"
        , backend = LibPgQuery
        , keywordCase = Lower
        , input = "create table t (a int, b text not null);\n"
        , expected = Output "create table t (\n    a int,\n    b text not null\n);\n"
        }
    , Case
        { name = "libpg-query: a comment it moves to the end does not swallow the semicolon"
        , backend = LibPgQuery
        , keywordCase = Lower
        , input = "SELECT a -- the a\nFROM t;\nSELECT 2;\n"
        , expected = Output "select a\nfrom t\n-- the a\n;\nselect 2;\n"
        }
    , Case
        { name = "auto: DDL to libpg_query, DML to the syntax backend"
        , backend = Auto
        , keywordCase = Lower
        , input = "create table t (a int, b text);\nselect a, b from t;\n"
        , expected = Output "create table t (\n    a int,\n    b text\n);\nselect\n    a\n  , b\nfrom t;\n"
        }
    , Case
        { name = "an indented statement keeps its indent"
        , backend = Syntax
        , keywordCase = Lower
        , input = "begin;\n    select a, b from t;\ncommit;\n"
        , expected = Output "begin;\n    select\n        a\n      , b\n    from t;\ncommit;\n"
        }
    , Case
        { name = "lines inside a string are not indented"
        , backend = LibPgQuery
        , keywordCase = Lower
        , input = "begin;\n    select 'a\nb';\ncommit;\n"
        , expected = Output "begin;\n    select 'a\nb';\ncommit;\n"
        }
    , Case
        { name = "comments around a statement stay put"
        , backend = Syntax
        , keywordCase = Lower
        , input = "/* before */ select a, b from t -- after\n;\n"
        , expected = Output "/* before */ select\n    a\n  , b\nfrom t -- after\n;\n"
        }
    , Case
        { name = "upper: keywords from both backends, identifiers spelled like keywords untouched"
        , backend = Auto
        , keywordCase = Upper
        , input = "create table t (name text, type int);\nselect name, type from t where name = 'x';\n"
        , expected = Output "CREATE TABLE t (\n    name text,\n    type int\n);\nSELECT\n    name\n  , type\nFROM t\nWHERE name = 'x';\n"
        }
    , Case
        { name = "not SQL"
        , backend = Auto
        , keywordCase = Lower
        , input = "\\set x 1\nselect :x;\n"
        , expected = Invalid
        }
    ]


main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let failWith n msg = do
            TIO.putStrLn ("FAIL: " <> n <> "\n" <> msg)
            modifyIORef' failures (+ 1)
    mapM_ (runCase failWith) cases
    n <- readIORef failures
    unless (n == 0) exitFailure
    putStrLn ("All " <> show (length cases) <> " file tests passed.")


runCase :: (Text -> Text -> IO ()) -> Case -> IO ()
runCase failWith c = do
    let opts = Options{backend = c.backend, dialect = defaultOptions.dialect, keywordCase = c.keywordCase, pretty = defaultOptions.pretty}
    result <- formatFile opts c.input
    case (c.expected, result) of
        (Invalid, Left _) -> pure ()
        (Invalid, Right r) -> failWith c.name ("expected a parse error, got:\n" <> r.output)
        (_, Left err) -> failWith c.name ("parse error: " <> T.pack (show err))
        (Unchanged reason, Right r) -> do
            unless (r.output == c.input) $ failWith c.name ("changed:\n" <> r.output)
            unless (all ((== Kept reason) . (.outcome)) r.reports) $
                failWith c.name ("reports: " <> T.pack (show r.reports))
        (Output out, Right r) -> do
            unless (r.output == out) $
                failWith c.name ("=== expected ===\n" <> out <> "=== actual ===\n" <> r.output)
            unless (not (any (isFailed . (.outcome)) r.reports)) $
                failWith c.name ("reports: " <> T.pack (show r.reports))
            -- Formatting is idempotent.
            again <- formatFile opts r.output
            case again of
                Right r' | r'.output == r.output -> pure ()
                Right r' -> failWith c.name ("not idempotent:\n" <> r'.output)
                Left err -> failWith c.name ("output does not parse: " <> T.pack (show err))
  where
    isFailed = \case
        Failed _ -> True
        _ -> False
