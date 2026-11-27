{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Monad (forM, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import PgStyle
import PgStyle.Haskell (Quoters, defaultQuoters, formatHaskell)
import PgStyle.LibPgQuery (LibPgQueryError (..), PrettyOptions (..), defaultPrettyOptions)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)


data Cli = Cli
    { check :: Bool
    , verbose :: Bool
    , options :: Options
    , haskell :: Bool
    , quoters :: Quoters
    , files :: [FilePath]
    }


cli :: Parser Cli
cli = do
    check <- switch (long "check" <> help "Change nothing; exit 1 if any file is not formatted")
    verbose <- switch (long "verbose" <> short 'v' <> help "Say which statements were left as they were, and why")
    backend <-
        option
            (eitherReader parseBackend)
            ( long "backend"
                <> metavar "auto|syntax|libpg-query"
                <> value Auto
                <> showDefaultWith (const "auto")
                <> help "What lays out statements: pgstyle's own renderer (DML), libpg_query's pretty-printer (anything), or the first where it applies and the second for the rest"
            )
    dialect <-
        option
            (eitherReader parseDialect)
            ( long "dialect"
                <> metavar "plain|hasql-th"
                <> value PlainSql
                <> showDefaultWith (const "plain")
                <> help "Accept hasql-th's extensions ($1 :: int8?, $Record {..}) in the syntax backend"
            )
    keywordCase <-
        option
            (eitherReader parseKeywordCase)
            ( long "keyword-case"
                <> metavar "lower|upper"
                <> value Lower
                <> showDefaultWith (const "lower")
                <> help "How to spell keywords in every formatted statement, whichever backend laid it out"
            )
    indentSize <- option auto (long "indent" <> metavar "N" <> value defaultPrettyOptions.indentSize <> showDefault <> help "libpg_query backend: spaces per indent level")
    maxLineLength <- option auto (long "width" <> metavar "N" <> value defaultPrettyOptions.maxLineLength <> showDefault <> help "libpg_query backend: line length to wrap lists at")
    haskell <- switch (long "haskell" <> help "Read stdin as Haskell, and format the SQL in its hasql-th quasiquotes (files ending in .hs always are)")
    extraQuoters <-
        many
            ( strOption
                ( long "quoter"
                    <> metavar "NAME"
                    <> help ("Haskell: also format quasiquotes of this quoter, besides " <> T.unpack (T.intercalate ", " defaultQuoters))
                )
            )
    files <- many (strArgument (metavar "FILE..." <> help "Files to format in place; none, or -, reads stdin and writes stdout"))
    pure
        Cli
            { check
            , verbose
            , options = Options{backend, dialect, keywordCase, pretty = PrettyOptions{indentSize, maxLineLength}}
            , haskell
            , quoters = defaultQuoters <> extraQuoters
            , files
            }


parseBackend :: String -> Either String Backend
parseBackend = \case
    "auto" -> Right Auto
    "syntax" -> Right Syntax
    "libpg-query" -> Right LibPgQuery
    other -> Left ("unknown backend " <> other)


parseKeywordCase :: String -> Either String KeywordCase
parseKeywordCase = \case
    "lower" -> Right Lower
    "upper" -> Right Upper
    other -> Left ("unknown keyword case " <> other)


parseDialect :: String -> Either String Dialect
parseDialect = \case
    "plain" -> Right PlainSql
    "hasql-th" -> Right HasqlTh
    other -> Left ("unknown dialect " <> other)


-- | How a file went, worst last.
data Status = Clean | Changed | Broken
    deriving stock (Eq, Ord)


main :: IO ()
main = do
    args <- execParser (info (cli <**> helper) (fullDesc <> progDesc "Format PostgreSQL files, and the SQL in Haskell files' hasql-th quasiquotes"))
    statuses <- case args.files of
        [] -> pure <$> run args "-"
        fs -> forM fs (run args)
    let worst = maximum (Clean : statuses)
    exitWith case worst of
        Clean -> ExitSuccess
        Changed | args.check -> ExitFailure 1
        Changed -> ExitSuccess
        Broken -> ExitFailure 2


run :: Cli -> FilePath -> IO Status
run args path = do
    input <- if path == "-" then TIO.getContents else TIO.readFile path
    result <-
        if isHaskell
            then pure (Right (formatHaskell args.quoters input))
            else formatFile args.options input
    case result of
        Left err -> do
            say (label <> ": not valid SQL" <> position err <> ": " <> err.message)
            pure Broken
        Right r -> do
            let failures = [rep | rep@StatementReport{outcome = Failed _} <- r.reports]
            mapM_ (report "error") failures
            when args.verbose $
                mapM_ (report "kept") [rep | rep@StatementReport{outcome = Kept _} <- r.reports]
            let changed = r.output /= input
            -- A file with a failed statement is still written: that statement
            -- is left as it was, and the others are fine.
            if
                | path == "-" && not args.check -> TIO.putStr r.output
                | changed && args.check -> say (label <> ": not formatted")
                | changed -> TIO.writeFile path r.output
                | otherwise -> pure ()
            pure
                if
                    | not (null failures) -> Broken
                    | changed -> Changed
                    | otherwise -> Clean
  where
    isHaskell = if path == "-" then args.haskell else ".hs" `T.isSuffixOf` T.pack path
    label = T.pack (if path == "-" then "<stdin>" else path)
    position err = if err.cursor > 0 then " at byte " <> T.pack (show err.cursor) else ""
    report :: Text -> StatementReport -> IO ()
    report kind rep =
        say (label <> ":" <> T.pack (show rep.line) <> ": " <> kind <> ": " <> describe rep.outcome)
    describe = \case
        Failed reason -> reason
        Kept reason -> reason
        FormattedBy b -> T.pack (show b)


say :: Text -> IO ()
say = TIO.hPutStrLn stderr
