-- | pgfmt's own layout for DML statements (SELECT, INSERT, UPDATE, DELETE,
-- VALUES), rendered from libpg_query's parse tree; and for CREATE TABLE,
-- CREATE VIEW, CREATE ... AS, CREATE INDEX, ALTER TABLE and CREATE FUNCTION,
-- in a style of its own ("PgFmt.Layout.Ddl").
--
-- This is the layout "PgFmt.Render" gives over postgresql-syntax's AST, and
-- it has to stay byte for byte the same: the rules (leading commas, one select
-- target per line when there are several, @join@ on its own line with @on@
-- under it, the first @where@ condition inline with hanging @and@s, ...) and
-- the thresholds are "PgFmt.Render"'s, function for function. Where
-- "PgFmt.Render" hands a node to postgresql-syntax's own renderer (and
-- lower-cases the result), this module writes that renderer's spelling too
-- (the 'MP' mode). It departs from "PgFmt.Render" to keep what the
-- source wrote (dollar-quoted strings, multi-line strings and numbers stay as
-- they are, comments are laid out rather than refused, and a blank line
-- between @values@ rows is kept), and in one rule:
-- @values@ rows that all fit go on one line each.
--
-- PostgreSQL's parse tree forgets some of what the source said that
-- postgresql-syntax's AST keeps: redundant parentheses, @!=@ for @<>@,
-- whether an alias had @as@, @inner join@ for @join@, how a type was spelled
-- (@int@, @integer@ and @int4@ are all @pg_catalog.int4@). Those come from
-- the source tokens, found through the tree's locations. Every parenthesis in
-- the source must be accounted for, as part of a construct's syntax or as
-- parentheses around one node; a statement with one that is not is refused,
-- as is any node this module does not know. Refused statements are left to
-- the caller (libpg_query's own printer, or as they are), never guessed at.
--
-- Comments are written at the line breaks the layout makes where they are
-- in the source (see 'brk'); a construct with a comment inside takes its
-- multi-line form, and a statement with a comment at no line break is
-- refused.
--
-- Keywords are written in the requested case as they are rendered;
-- identifiers, quoted or not, are never changed.
--
-- The parts: "PgFmt.Layout.Monad" holds the statement's tokens and what is
-- placed so far; "PgFmt.Layout.Comments", "PgFmt.Layout.Locations" and
-- "PgFmt.Layout.Parens" find comments, locations and parentheses in the
-- source; "PgFmt.Layout.Names" writes identifiers, constants and types;
-- "PgFmt.Layout.Expr" expressions, "PgFmt.Layout.Dml" selects and the other
-- DML statements, and "PgFmt.Layout.Ddl" the DDL ones. This module says
-- which statement goes where.
module PgFmt.Layout (
    formatStatement,
    formatStatementAt,
) where

import Control.Monad (unless)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State.Strict (runStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.ProtoLens.Message (Message (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Lens.Micro ((^.))
import PgFmt.LibPgQuery qualified as L
import PgQuery qualified as Pg
import Prettyprinter (layoutPretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import PgFmt.Layout.Ddl
import PgFmt.Layout.Dml
import PgFmt.Layout.Expr
import PgFmt.Layout.Monad
import PgFmt.Layout.Names
import PgFmt.Layout.Parens


-- | Lay out one DML statement or one of the DDL statements it knows (no comments
-- before or after it, no semicolon), keywords in upper case if asked,
-- starting at column 0 without a trailing newline. 'Left' says why not: the
-- statement is none of those, or has
-- something this renderer does not handle (a comment it cannot place among
-- them).
formatStatement :: Bool -> ByteString -> IO (Either Text Text)
formatStatement upperKeywords = formatStatementAt upperKeywords 0


-- | 'formatStatement' for a statement that starts at this column of its
-- line, where the caller indents its other lines to: what lines up with
-- text the layout cannot move (a function body's closing quote) needs it.
formatStatementAt :: Bool -> Int -> ByteString -> IO (Either Text Text)
formatStatementAt upperKeywords column sql = do
    parsed <- L.parse sql
    scanned <- L.scan sql
    let laid = render parsed scanned
    case laid of
        Right out | Right scan <- scanned, hasComments scan -> do
            -- Every comment must come out once, in its order: each is
            -- written where the layout breaks a line at its place in the
            -- source, and one with no such break is not written at all.
            again <- L.scan (TE.encodeUtf8 out)
            pure case again of
                Right scan' | commentTexts (TE.encodeUtf8 out) scan' == commentTexts sql scan -> laid
                _ -> Left "a comment pgfmt cannot place"
        _ -> pure laid
  where
    render parsed scanned = case (parsed, scanned) of
        (Left err, _) -> Left err.message
        (_, Left err) -> Left err.message
        (Right tree, Right scan) -> case tree ^. Pg.stmts of
            [rawStmt] | Just stmt <- rawStmt ^. Pg.maybe'stmt -> do
                let env = (mkEnv upperKeywords sql scan stmt){column}
                (doc, st) <- runStateT (runReaderT (statement stmt) env) St{consumed = IS.empty, parenCounts = Map.empty, boolGroups = IS.empty}
                let unplaced = [i | (i, t) <- IM.toList env.toks, t.text == "(", not (IS.member i st.consumed)]
                unless (null unplaced) (Left "a parenthesis pgfmt cannot place")
                Right (renderStrict (layoutPretty layoutOpts doc))
            _ -> Left "not a single statement"
    hasComments scan = any isComment (scan ^. Pg.tokens)
    commentTexts src scan =
        [ BS.take (fromIntegral (t ^. Pg.end) - fromIntegral (t ^. Pg.start)) (BS.drop (fromIntegral (t ^. Pg.start)) src)
        | t <- scan ^. Pg.tokens
        , isComment t
        ]


-- | A statement, by its kind.
statement :: Pg.Node -> R D
statement n = case node n of
    Just (Pg.Node'SelectStmt s) -> do
        k <- selectParens 0 s
        selStmt s k
    Just (Pg.Node'InsertStmt s) -> insertStmt s
    Just (Pg.Node'UpdateStmt s) -> updateStmt s
    Just (Pg.Node'DeleteStmt s) -> deleteStmt s
    Just (Pg.Node'CreateStmt c) -> local (\env -> env{tightTypes = True}) (createTable c)
    Just (Pg.Node'ViewStmt v) -> createView v
    Just (Pg.Node'IndexStmt ix) -> createIndex ix
    Just (Pg.Node'CreateFunctionStmt f) -> local (\env -> env{tightTypes = True}) (createFunction statement f)
    Just (Pg.Node'AlterTableStmt a) -> local (\env -> env{tightTypes = True}) (alterTable a)
    Just (Pg.Node'CreateTableAsStmt c) -> createTableAs c
    Just (Pg.Node'CreateTrigStmt t) -> createTrigger t
    Just (Pg.Node'GrantStmt _) -> grantStmt True
    Just (Pg.Node'GrantRoleStmt _) -> grantStmt False
    Just (Pg.Node'CallStmt c) -> do
        f <- req "a call" (c ^. Pg.maybe'funccall)
        e <- top (defMessage `withFuncCall` f)
        k <- kw "call"
        d <- funcApplication MP e f
        pure (k <+> d)
    _ -> unsupported "not DML or DDL pgfmt lays out"
