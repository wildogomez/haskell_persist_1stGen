{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Abeson
    ( AbesonConfig(..)
    , toAeson, toAesonValue
    , toBson,  toBsonValue
    -- * reexport
    , def
    ) where

import Data.Default.Class
import qualified Data.Aeson as A
import qualified Data.Bson  as B

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as Base64

import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as LB

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.UUID as UUID
import Data.Monoid
import Data.Scientific
import Data.Int
import Data.Bits
import Data.Time.Clock.POSIX

data AbesonConfig = AbesonConfig
    { binaryEncoding     :: AbesonConfig -> S.ByteString -> A.Value
    , functionEncoding   :: AbesonConfig -> S.ByteString -> A.Value
    , userDefEncoding    :: AbesonConfig -> S.ByteString -> A.Value
    , objectIdEncoding   :: AbesonConfig -> B.ObjectId   -> A.Value
    , regexEncoding      :: AbesonConfig -> B.Regex      -> A.Value
    , javascriptEncoding :: AbesonConfig -> B.Javascript -> A.Value
    , stampEncoding      :: AbesonConfig -> Int64 -> A.Value
    , minMaxKeyEncoding  :: AbesonConfig -> B.MinMaxKey -> A.Value
    }

-- | binary, function, userDef -> base64 encoding
--
-- objectid -> show
--
-- regex -> \/pat\/mod
--
-- javascript -> {environment: env, code: code}
--
-- stamp -> {t: time_t, i: ordinal}
--
-- minkey -> -1, maxkey -> 1
instance Default AbesonConfig where
    def = AbesonConfig
        { binaryEncoding   = const $ A.String . T.decodeUtf8 . Base64.encode
        , functionEncoding = const $ A.String . T.decodeUtf8 . Base64.encode
        , userDefEncoding  = const $ A.String . T.decodeUtf8 . Base64.encode
        , objectIdEncoding = const $ A.String . T.pack . show
        , regexEncoding    = \_ (B.Regex p m) ->
            A.String . TL.toStrict . LB.toLazyText $
            LB.singleton '/' <> LB.fromText p <> LB.singleton '/' <> LB.fromText m
        , javascriptEncoding = \c (B.Javascript env bdy) ->
            A.object [ "environment" A..= A.Object (toAeson c env)
                     , "code"        A..= A.String bdy
                     ]
        , stampEncoding = const defaultStampEncode
        , minMaxKeyEncoding = const $ \case
            B.MinKey -> A.toJSON (-1 :: Int)
            B.MaxKey -> A.toJSON ( 1 :: Int)
        }

defaultStampEncode :: Int64 -> A.Value
defaultStampEncode i = A.object
    [ "t" A..= posixSecondsToUTCTime (fromIntegral $ shiftR i 32)
    , "i" A..= (i .&. 0xffff)
    ]

-- para bson a aeson
toAeson :: AbesonConfig -> B.Document -> A.Object
toAeson c d = H.fromList $
    map (\(label B.:= value) -> (label, toAesonValue c value)) d

toAesonValue :: AbesonConfig -> B.Value -> A.Value
toAesonValue _ (B.Float  b) = A.Number $ fromFloatDigits b
toAesonValue _ (B.String b) = A.String b
toAesonValue c (B.Doc    b) = A.Object $ toAeson c b
toAesonValue c (B.Array  b) = A.Array . V.fromList $ map (toAesonValue c) b
toAesonValue c (B.Bin (B.Binary   b)) = binaryEncoding   c c b
toAesonValue c (B.Fun (B.Function b)) = functionEncoding c c b
toAesonValue _ (B.Uuid (B.UUID b)) = case UUID.fromByteString (L.fromStrict b) of
    Nothing -> error ""
    Just ui -> (A.String . T.decodeUtf8 . UUID.toASCIIBytes) ui
toAesonValue _ (B.Md5 (B.MD5 b)) = A.String $ T.decodeUtf8 b
toAesonValue c (B.UserDef (B.UserDefined b)) = userDefEncoding c c b
toAesonValue c (B.ObjId b) = objectIdEncoding c c b
toAesonValue _ (B.Bool  b) = A.Bool b
toAesonValue _ (B.UTC   b) = A.toJSON b
toAesonValue _  B.Null     = A.Null
toAesonValue c (B.RegEx b) = regexEncoding c c b
toAesonValue c (B.JavaScr b) = javascriptEncoding c c b
toAesonValue _ (B.Sym (B.Symbol b)) = A.String b
toAesonValue _ (B.Int32 b) = A.toJSON b
toAesonValue _ (B.Int64 b) = A.toJSON b
toAesonValue c (B.Stamp (B.MongoStamp b)) = stampEncoding c c b
toAesonValue c (B.MinMax b) = minMaxKeyEncoding c c b


-- para aeson a bson
toBson :: A.Object -> B.Document
toBson = map (\(k,v) -> k B.:= toBsonValue v) . H.toList

toBsonValue :: A.Value -> B.Value
toBsonValue (A.Object a) = B.Doc $ toBson a
toBsonValue (A.Array  a) = B.Array $ map toBsonValue (V.toList a)
toBsonValue (A.String a) = B.String a
toBsonValue (A.Number a) = case floatingOrInteger a of
    Left  f -> B.Float f
    Right i -> B.Int64 i
toBsonValue (A.Bool a)   = B.Bool a
toBsonValue  A.Null      = B.Null
