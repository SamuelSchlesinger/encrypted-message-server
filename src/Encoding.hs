{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Encoding where

import Servant.API.ContentTypes
import Data.Coerce (coerce)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import qualified Data.Binary as Binary
import Data.Function ((&))

class ToByteString a where
  toByteString :: a -> LBS.ByteString

instance ToByteString LBS.ByteString where
  toByteString = id

instance ToByteString SBS.ByteString where
  toByteString = LBS.fromStrict

class FromByteString a where
  fromByteString :: LBS.ByteString -> Either String a

instance FromByteString LBS.ByteString where
  fromByteString = Right

instance FromByteString SBS.ByteString where
  fromByteString = Right . LBS.toStrict

instance FromByteString a => MimeUnrender OctetStream a where
  mimeUnrender _ = fromByteString

instance ToByteString a => MimeRender OctetStream a where
  mimeRender _ = toByteString

newtype BinaryNewtype a = BinaryNewtype a

instance Binary.Binary a => ToByteString (BinaryNewtype a) where
  toByteString (BinaryNewtype a) = Binary.encode a

instance Binary.Binary a => FromByteString (BinaryNewtype a) where
  fromByteString bs =
    Binary.decodeOrFail bs
    & \case
      Left (rest, byteOffset, errorMessage) -> Left $ "Failed parsing after " <> show byteOffset <> " bytes, with error message: " <> errorMessage
      Right (rest, byteOffset, result) -> if LBS.null rest then Right (BinaryNewtype result) else Left $ "Succeeded parsing after " <> show byteOffset <> " bytes, but had more input to consume"

deriving via (BinaryNewtype [a]) instance Binary.Binary a => ToByteString [a]
deriving via (BinaryNewtype [a]) instance Binary.Binary a => FromByteString [a]
