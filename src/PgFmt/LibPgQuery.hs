-- | The libpg_query calls pgfmt needs that the @libpg-query@ package does not
-- expose: parsing and scanning bytes rather than a 'String' (so statement
-- locations are byte offsets into exactly what we passed), and pretty-printed
-- deparsing with the source's comments put back.
--
-- The C side is @cbits/pgfmt.c@.
module PgFmt.LibPgQuery (
    LibPgQueryError (..),
    parse,
    parsePlpgsql,
    scan,
    deparse,
    PrettyOptions (..),
    defaultPrettyOptions,
    deparsePretty,
    Statement (..),
    statements,
    Token (..),
    tokens,
    canonical,
) where

import Control.Exception (finally, mask_)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Foldable (toList)
import Data.ProtoLens (Message, decodeMessage, encodeMessage)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign (Ptr, alloca, free, nullPtr, peek, poke)
import Foreign.C (CInt (..), CSize (..), CString)
import Lens.Micro ((^.))
import PgQuery qualified as Pg


-- | An error libpg_query reported. @cursor@ is the 1-based byte position in
-- the input, or 0 when it gave none.
data LibPgQueryError = LibPgQueryError
    { message :: Text
    , cursor :: Int
    }
    deriving stock (Eq, Show)


type Out = Ptr CString -> Ptr CSize -> Ptr CInt -> IO CInt


foreign import ccall safe "pgfmt_parse"
    c_parse :: CString -> Out


foreign import ccall safe "pgfmt_parse_json"
    c_parse_json :: CString -> Out


foreign import ccall safe "pgfmt_parse_plpgsql"
    c_parse_plpgsql :: CString -> Out


foreign import ccall safe "pgfmt_scan"
    c_scan :: CString -> Out


foreign import ccall safe "pgfmt_deparse"
    c_deparse :: CString -> CSize -> CInt -> CInt -> CInt -> CString -> Out


-- | Run a shim call and take ownership of the buffer it hands back.
runOut :: Out -> IO (Either LibPgQueryError ByteString)
runOut call =
    alloca \pOut -> alloca \pLen -> alloca \pCursor -> do
        poke pOut nullPtr
        -- Masked from the call until the buffer is copied and freed, so an
        -- asynchronous exception cannot leave it allocated.
        (rc, bytes, cur) <- mask_ do
            rc <- call pOut pLen pCursor
            buf <- peek pOut
            len <- peek pLen
            cur <- peek pCursor
            bytes <-
                if buf == nullPtr
                    then pure BS.empty
                    else BS.packCStringLen (buf, fromIntegral len) `finally` free buf
            pure (rc, bytes, cur)
        pure
            if rc == 0
                then Right bytes
                else Left LibPgQueryError{message = decodeUtf8 bytes, cursor = fromIntegral cur}


-- | A NUL-terminated copy of the input. libpg_query reads C strings, so a NUL
-- inside the input would end it early; SQL cannot contain one anyway.
withSql :: ByteString -> (CString -> IO a) -> IO a
withSql sql = BS.useAsCString sql


decodeProto :: Message msg => Either LibPgQueryError ByteString -> Either LibPgQueryError msg
decodeProto = \case
    Left err -> Left err
    Right bytes -> case decodeMessage bytes of
        Left err -> Left LibPgQueryError{message = "protobuf: " <> T.pack err, cursor = 0}
        Right msg -> Right msg


-- | PostgreSQL's parse tree of UTF-8 SQL.
parse :: ByteString -> IO (Either LibPgQueryError Pg.ParseResult)
parse sql = decodeProto <$> withSql sql (runOut . c_parse)


-- | libpg_query's JSON for the PL/pgSQL functions UTF-8 SQL creates: an
-- array with one @PLpgSQL_function@ per function, empty when it creates none.
parsePlpgsql :: ByteString -> IO (Either LibPgQueryError ByteString)
parsePlpgsql sql = withSql sql (runOut . c_parse_plpgsql)


-- | PostgreSQL's token stream of UTF-8 SQL, comments included.
scan :: ByteString -> IO (Either LibPgQueryError Pg.ScanResult)
scan sql = decodeProto <$> withSql sql (runOut . c_scan)


-- | Compact SQL for a parse tree: libpg_query's normal form, one line per
-- statement.
deparse :: Pg.ParseResult -> IO (Either LibPgQueryError Text)
deparse tree =
    fmap decodeUtf8
        <$> unsafeUseAsCStringLen (encodeMessage tree) \(ptr, len) ->
            runOut (c_deparse ptr (fromIntegral len) 0 0 0 nullPtr)


-- | libpg_query's pretty-printer settings.
data PrettyOptions = PrettyOptions
    { indentSize :: Int
    , maxLineLength :: Int
    }
    deriving stock (Eq, Show)


defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions = PrettyOptions{indentSize = 4, maxLineLength = 80}


-- | Pretty-printed SQL for a parse tree. With the source the tree was parsed
-- from, its comments are put back where they were.
deparsePretty :: PrettyOptions -> Maybe ByteString -> Pg.ParseResult -> IO (Either LibPgQueryError Text)
deparsePretty opts commentSource tree =
    fmap decodeUtf8 <$> unsafeUseAsCStringLen (encodeMessage tree) \(ptr, len) ->
        let call src =
                runOut
                    ( c_deparse
                        ptr
                        (fromIntegral len)
                        1
                        (fromIntegral opts.indentSize)
                        (fromIntegral opts.maxLineLength)
                        src
                    )
         in case commentSource of
                Nothing -> call nullPtr
                Just source -> withSql source call


-- | One top-level statement of a parsed source, as byte offsets into it.
-- @end@ is exclusive and stops before the terminating semicolon. The span
-- starts right after the previous statement's semicolon, so it includes the
-- whitespace and comments in front of the statement.
data Statement = Statement
    { start :: Int
    , end :: Int
    , raw :: Pg.RawStmt
    }


statements :: ByteString -> Pg.ParseResult -> [Statement]
statements source tree = map toStatement (tree ^. Pg.stmts)
  where
    toStatement raw =
        let start = fromIntegral (raw ^. Pg.stmtLocation)
            len = fromIntegral (raw ^. Pg.stmtLen)
            -- 0 means "to the end of the input": the last statement, with no
            -- semicolon after it.
            end = if len == 0 then BS.length source else start + len
         in Statement{start, end, raw}


-- | A token of the source, as byte offsets into it, end exclusive.
data Token = Token
    { start :: Int
    , end :: Int
    , isComment :: Bool
    , isKeyword :: Bool
    -- ^ Spelled like a keyword. Unreserved keywords can also be identifiers
    -- (@name@, @type@), which the scanner cannot tell apart.
    }
    deriving stock (Eq, Show)


tokens :: Pg.ScanResult -> [Token]
tokens result =
    [ Token
        { start = fromIntegral (t ^. Pg.start)
        , end = fromIntegral (t ^. Pg.end)
        , isComment = t ^. Pg.token `elem` [Pg.SQL_COMMENT, Pg.C_COMMENT]
        , isKeyword = t ^. Pg.keywordKind /= Pg.NO_KEYWORD
        }
    | t <- result ^. Pg.tokens
    ]


-- | The normal form of each statement in UTF-8 SQL: its parse tree, from
-- libpg_query's JSON output, with every source position removed: @location@
-- and @name_location@, @stmt_location@ and @stmt_len@, and where a list's
-- brackets are (@list_start@, @list_end@, and @rexpr_list_start@ and
-- @rexpr_list_end@ for @IN@ lists). Two statements with the same
-- normal form are the same statement to PostgreSQL, however they are laid out.
--
-- This compares trees, not text: two trees the deparser happens to write the
-- same way still count as different. libpg_query 18 leaves empty strings out
-- of its trees, so @comment on t is ''@ and @comment on t is null@ are the
-- same tree to it; PostgreSQL treats them the same too.
canonical :: ByteString -> IO (Either LibPgQueryError [Value])
canonical sql = do
    json <- withSql sql (runOut . c_parse_json)
    pure do
        bytes <- json
        case Aeson.eitherDecodeStrict bytes of
            Left err -> Left LibPgQueryError{message = "libpg_query's JSON does not decode: " <> T.pack err, cursor = 0}
            Right v -> Right (statementsOf v)
  where
    statementsOf = \case
        Object o | Just (Array xs) <- KeyMap.lookup "stmts" o -> map withoutPositions (toList xs)
        _ -> []
    withoutPositions = \case
        Object o -> Object (KeyMap.map withoutPositions (KeyMap.filterWithKey (\k _ -> not (isPosition (Key.toText k))) o))
        Array xs -> Array (fmap withoutPositions xs)
        v -> v
    isPosition k =
        k == "location"
            || "_location" `T.isSuffixOf` k
            || k == "stmt_len"
            || "list_start" `T.isSuffixOf` k
            || "list_end" `T.isSuffixOf` k


decodeUtf8 :: ByteString -> Text
decodeUtf8 = TE.decodeUtf8With TE.lenientDecode
