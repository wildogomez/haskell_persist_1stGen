{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module AbesonV2
    ( 
      toAeson, toAesonValue
    , toBson,  toBsonValue
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

-- Convertir un valor BSON a un valor AESON
toAesonValue :: B.Value -> A.Value
toAesonValue (B.Float  b) = A.Number $ fromFloatDigits b
toAesonValue (B.String b) = A.String b
toAesonValue (B.Doc    doc) = A.Object $ toAeson doc
toAesonValue (B.Array list) = A.Array . V.fromList $ map toAesonValue list

toAesonValue (B.Bin (B.Binary b)) = (A.String . T.decodeUtf8 . Base64.encode) b
toAesonValue (B.Fun (B.Function b)) = (A.String . T.decodeUtf8 . Base64.encode) b
toAesonValue (B.Uuid (B.UUID b)) = case UUID.fromByteString (L.fromStrict b) of
    Nothing -> error ""
    Just ui -> (A.String . T.decodeUtf8 . UUID.toASCIIBytes) ui
toAesonValue (B.Md5 (B.MD5 b)) = A.String $ T.decodeUtf8 b
toAesonValue (B.UserDef (B.UserDefined userDef)) = (A.String . T.decodeUtf8 . Base64.encode) userDef
toAesonValue (B.ObjId oid) = (A.String . T.pack . show) oid
toAesonValue (B.Bool  b) = A.Bool b
toAesonValue (B.UTC b) = A.toJSON b
toAesonValue (B.Null) = A.Null
toAesonValue (B.Sym (B.Symbol sym)) = A.String sym
toAesonValue (B.JavaScr (B.Javascript env code)) = 
            A.object [ "environment" A..= A.Object (toAeson env)
                     , "code"        A..= A.String code
                     ]
toAesonValue (B.RegEx (B.Regex p m)) = A.String . TL.toStrict . LB.toLazyText $
            LB.singleton '/' <> LB.fromText p <> LB.singleton '/' <> LB.fromText m
toAesonValue (B.Int32 b) = A.toJSON b
toAesonValue (B.Int64 b) = A.toJSON b
toAesonValue (B.MinMax mm) = case mm of { B.MinKey -> A.toJSON (-1 :: Int)
                                       ; B.MaxKey -> A.toJSON (1 :: Int)}
                                                                
toAeson :: B.Document -> A.Object
toAeson = H.fromList . map (\(l B.:= v) -> (l, toAesonValue v))
                                    
toBson :: A.Object -> B.Document
toBson = map (\(k,v) -> k B.:= toBsonValue v) . H.toList

-- Convertimos de AESON Value a BSON Value
toBsonValue :: A.Value -> B.Value
toBsonValue (A.Object a) = B.Doc $ toBson a
toBsonValue (A.Array  a) = B.Array $ map toBsonValue (V.toList a)
toBsonValue (A.String a) = B.String a
toBsonValue (A.Number a) = case floatingOrInteger a of
    Left  f -> B.Float f
    Right i -> B.Int64 i
toBsonValue (A.Bool a)   = B.Bool a
toBsonValue  A.Null      = B.Null
