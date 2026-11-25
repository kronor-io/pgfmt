-- | The libpg_query calls pgfmt needs that the @libpg-query@ package does not
-- expose: parsing and scanning bytes rather than a 'String' (so statement
-- locations are byte offsets into exactly what we passed), and pretty-printed
-- deparsing with the source's comments put back.
--
-- The C side is @cbits/pgfmt.c@.
module PgFmt.LibPgQuery (
    LibPgQueryError (..),
    parse,
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

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Foreign (Ptr, alloca, free, nullPtr, peek)
import Foreign.C (CInt (..), CSize (..), CString)
import Lens.Micro ((&), (.~), (^.))
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


foreign import ccall safe "pgfmt_scan"
    c_scan :: CString -> Out


foreign import ccall safe "pgfmt_deparse"
    c_deparse :: CString -> CSize -> CInt -> CInt -> CInt -> CString -> Out


-- | Run a shim call and take ownership of the buffer it hands back.
runOut :: Out -> IO (Either LibPgQueryError ByteString)
runOut call =
    alloca \pOut -> alloca \pLen -> alloca \pCursor -> do
        rc <- call pOut pLen pCursor
        buf <- peek pOut
        len <- peek pLen
        cur <- peek pCursor
        bytes <-
            if buf == nullPtr
                then pure BS.empty
                else BS.packCStringLen (buf, fromIntegral len) <* free buf
        pure
            if rc == 0
                then Right bytes
                else Left LibPgQueryError{message = decodeUtf8 bytes, cursor = fromIntegral cur}


-- | A NUL-terminated copy of the input. libpg_query reads C strings, so a NUL
-- inside the input would end it early; SQL cannot contain one anyway.
withSql :: ByteString -> (CString -> IO a) -> IO a
withSql sql = BS.useAsCString sql


decodeProto :: (Message msg) => Either LibPgQueryError ByteString -> Either LibPgQueryError msg
decodeProto = \case
    Left err -> Left err
    Right bytes -> case decodeMessage bytes of
        Left err -> Left LibPgQueryError{message = "protobuf: " <> T.pack err, cursor = 0}
        Right msg -> Right msg


-- | PostgreSQL's parse tree of UTF-8 SQL.
parse :: ByteString -> IO (Either LibPgQueryError Pg.ParseResult)
parse sql = decodeProto <$> withSql sql (runOut . c_parse)


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
    }
    deriving stock (Eq, Show)


tokens :: Pg.ScanResult -> [Token]
tokens result =
    [ Token
        { start = fromIntegral (t ^. Pg.start)
        , end = fromIntegral (t ^. Pg.end)
        , isComment = t ^. Pg.token `elem` [Pg.SQL_COMMENT, Pg.C_COMMENT]
        }
    | t <- result ^. Pg.tokens
    ]


-- | A statement's normal form: libpg_query's compact deparse of it alone,
-- with its location stripped. Two statements with the same normal form are
-- the same statement to PostgreSQL, however they are laid out.
canonical :: Pg.RawStmt -> IO (Either LibPgQueryError Text)
canonical raw =
    deparse
        ( defMessage
            & Pg.stmts
                .~ [ raw
                        & Pg.stmtLocation .~ 0
                        & Pg.stmtLen .~ 0
                   ]
        )


decodeUtf8 :: ByteString -> Text
decodeUtf8 = TE.decodeUtf8With TE.lenientDecode
