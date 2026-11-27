module Main (main) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PgStyle
import PgStyle.Haskell (Quoters, defaultQuoters, formatHaskell)
import System.Exit (exitFailure)


data Case = Case
    { name :: Text
    , quoters :: Quoters
    , input :: [Text]
    , output :: [Text]
    , reports :: [(Int, Outcome)]
    -- ^ The line of each quasiquote of 'quoters', and what happened to it.
    }


-- | Each quasiquote below opens at column 4, so its SQL is laid out at
-- column 8 and its closing bar at column 4.
cases :: [Case]
cases =
    [ Case
        { name = "a quasiquote is laid out under its opening bracket"
        , quoters = defaultQuoters
        , input =
            [ "q = [singletonStatement|"
            , "  select a :: int8, b :: text from t where id = $1 :: int8"
            , "  |]"
            ]
        , output =
            [ "q = [singletonStatement|"
            , "        select"
            , "            a::int8"
            , "          , b::text"
            , "        from t"
            , "        where id = $1::int8"
            , "    |]"
            ]
        , reports = [(1, FormattedBy Syntax)]
        }
    , Case
        { name = "quasiquotes in comments and strings are not quasiquotes"
        , quoters = defaultQuoters
        , input =
            [ "-- [singletonStatement|"
            , "-- select 1 |]"
            , "{- {- nested [vectorStatement|"
            , "select 1 |] -} -}"
            , "a = \"[maybeStatement|\\"
            , "    \\select 1 |]\""
            ]
        , output =
            [ "-- [singletonStatement|"
            , "-- select 1 |]"
            , "{- {- nested [vectorStatement|"
            , "select 1 |] -} -}"
            , "a = \"[maybeStatement|\\"
            , "    \\select 1 |]\""
            ]
        , reports = []
        }
    , Case
        { name = "quotes in character literals, primes and quoted names do not start strings"
        , quoters = defaultQuoters
        , input =
            [ "c = '\"'"
            , "e = '\\''"
            , "x' = ''Int"
            , "(-->) = 'id"
            , "r = [vectorStatement|"
            , "  select a :: int8 from t"
            , "  |]"
            ]
        , output =
            [ "c = '\"'"
            , "e = '\\''"
            , "x' = ''Int"
            , "(-->) = 'id"
            , "r = [vectorStatement|"
            , "        select a::int8"
            , "        from t"
            , "    |]"
            ]
        , reports = [(5, FormattedBy Syntax)]
        }
    , Case
        { name = "another quoter's body is skipped, quotes and all; a qualified quoter counts"
        , quoters = defaultQuoters
        , input =
            [ "g = [i|it's \"odd |]"
            , "r = [Hasql.TH.maybeStatement|"
            , "  select a :: int8 from t"
            , "  |]"
            ]
        , output =
            [ "g = [i|it's \"odd |]"
            , "r = [Hasql.TH.maybeStatement|"
            , "        select a::int8"
            , "        from t"
            , "    |]"
            ]
        , reports = [(2, FormattedBy Syntax)]
        }
    , Case
        { name = "a quoter added to the defaults is formatted"
        , quoters = defaultQuoters <> ["sql"]
        , input =
            [ "s = [sql|"
            , "  select a :: int8 from t"
            , "  |]"
            ]
        , output =
            [ "s = [sql|"
            , "        select a::int8"
            , "        from t"
            , "    |]"
            ]
        , reports = [(1, FormattedBy Syntax)]
        }
    , Case
        { name = "single-line and unparsed quasiquotes are kept; the others are still formatted"
        , quoters = defaultQuoters
        , input =
            [ "a = [singletonStatement|select 1 :: int4|]"
            , "b = [uncheckedSql|"
            , "  create table t (a int)"
            , "  |]"
            , "c = [singletonStatement|"
            , "  select 1 :: int4"
            , "  |]"
            ]
        , output =
            [ "a = [singletonStatement|select 1 :: int4|]"
            , "b = [uncheckedSql|"
            , "  create table t (a int)"
            , "  |]"
            , "c = [singletonStatement|"
            , "        select 1::int4"
            , "    |]"
            ]
        , reports =
            [ (1, Kept "single-line quasiquotes are left as they are")
            , (2, Kept "postgresql-syntax does not parse it")
            , (5, FormattedBy Syntax)
            ]
        }
    , Case
        { name = "an opening with no closing bar is a list comprehension"
        , quoters = defaultQuoters <> ["x"]
        , input = ["xs = [x|x <- [1, 2]]"]
        , output = ["xs = [x|x <- [1, 2]]"]
        , reports = []
        }
    ]


main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let failWith label lines' = do
            modifyIORef' failures (+ 1)
            TIO.putStrLn ("FAIL: " <> label)
            mapM_ TIO.putStrLn lines'
    mapM_
        ( \c -> do
            let source = T.unlines c.input
                expected = T.unlines c.output
                r = formatHaskell c.quoters source
                got = [(rep.line, rep.outcome) | rep <- r.reports]
                again = formatHaskell c.quoters r.output
            unless (r.output == expected) $
                failWith c.name ["=== expected ===", expected, "=== actual ===", r.output]
            unless (got == c.reports) $
                failWith (c.name <> " (reports)") [T.pack (show got)]
            unless (again.output == r.output) $
                failWith (c.name <> " (not idempotent)") [again.output]
        )
        cases
    n <- readIORef failures
    if n == 0
        then putStrLn ("All " <> show (length cases) <> " Haskell tests passed.")
        else exitFailure
