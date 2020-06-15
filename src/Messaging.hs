{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Messaging where

import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as RSA.OAEP
import qualified Crypto.PubKey.RSA.PSS as RSA.PSS
import Crypto.PubKey.RSA.OAEP (defaultOAEPParams)
import Crypto.PubKey.RSA.PSS (defaultPSSParams)
import Crypto.Hash (SHA512(SHA512))
import qualified Data.ByteString.Char8 as BS8
import qualified GHC.Generics
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (when)
import Data.Coerce (coerce)
import qualified Data.Binary as Binary
import Data.Function ((&))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))

import Encoding

newtype PrivateKey = PrivateKey { unPrivateKey :: RSA.PrivateKey }
  deriving newtype (Eq, Show, Read)
  deriving (ToByteString, FromByteString) via BinaryNewtype PrivateKey

newtype PublicKey = PublicKey { unPublicKey :: RSA.PublicKey }
  deriving newtype (Eq, Show, Read)
  deriving (ToByteString, FromByteString) via BinaryNewtype PublicKey

instance Binary.Binary PrivateKey where
  put (PrivateKey (RSA.PrivateKey private_pub private_d private_p private_q private_dP private_dQ private_qinv)) = do
    Binary.put (PublicKey private_pub)
    Binary.put private_d
    Binary.put private_p
    Binary.put private_q
    Binary.put private_dP
    Binary.put private_dQ
    Binary.put private_qinv
  get = PrivateKey <$>
    (RSA.PrivateKey 
      <$> (unPublicKey <$> Binary.get) 
      <*> Binary.get
      <*> Binary.get
      <*> Binary.get
      <*> Binary.get
      <*> Binary.get
      <*> Binary.get
    )

instance Binary.Binary PublicKey where
  put (PublicKey RSA.PublicKey{..}) = do
    Binary.put public_size
    Binary.put public_n
    Binary.put public_e
  get = PublicKey
    <$> (RSA.PublicKey
      <$> Binary.get
      <*> Binary.get
      <*> Binary.get
        )

data Signed a = Signed
  { signedBy :: PublicKey
  , signature :: SBS.ByteString
  , thing :: a }
  deriving stock (GHC.Generics.Generic, Show, Read)
  deriving anyclass Binary.Binary

data Message a = Message
  { encryptedFor :: PublicKey
  , encrypted :: Signed (Encrypted a) }
  deriving stock (GHC.Generics.Generic, Show, Read)
  deriving anyclass Binary.Binary

data MessageResponse = MessageResponse
  { serverPublicKey :: PublicKey }
  deriving stock (GHC.Generics.Generic, Show, Read)
  deriving anyclass Binary.Binary

deriving via (BinaryNewtype (Message a)) instance Binary.Binary a => ToByteString (Message a)
deriving via (BinaryNewtype (Signed a)) instance Binary.Binary a => ToByteString (Signed a)
deriving via (BinaryNewtype (Encrypted a)) instance Binary.Binary a => ToByteString (Encrypted a)
deriving via (BinaryNewtype MessageResponse) instance ToByteString MessageResponse

deriving via (BinaryNewtype (Message a)) instance Binary.Binary a => FromByteString (Message a)
deriving via (BinaryNewtype (Signed a)) instance Binary.Binary a => FromByteString (Signed a)
deriving via (BinaryNewtype (Encrypted a)) instance Binary.Binary a => FromByteString (Encrypted a)
deriving via (BinaryNewtype MessageResponse) instance FromByteString MessageResponse

newtype Encrypted a = Encrypted { unsafeUnDecrypted :: SBS.ByteString }
  deriving newtype (Binary.Binary, Show, Read)

writeTo :: (Binary.Binary a) => PrivateKey -> PublicKey -> a -> IO (Maybe (Message a))
writeTo privateKey publicKey a = runMaybeT do
  encrypted <- MaybeT (encrypt publicKey a)
  signed <- MaybeT (sign privateKey encrypted)
  pure (Message publicKey signed)

readMessage :: (FromByteString a, Binary.Binary a) => PrivateKey -> Message a -> Maybe a
readMessage privateKey message = decrypt privateKey =<< verify (encrypted message)
  
publicOfPrivate :: PrivateKey -> PublicKey
publicOfPrivate (PrivateKey privateKey) = coerce $ RSA.private_pub privateKey

decrypt :: (Binary.Binary a, FromByteString a) => PrivateKey -> Encrypted a -> Maybe a
decrypt (PrivateKey privateKey) (Encrypted sbs) =
  either (const Nothing) 
         (either (const Nothing) Just . fromByteString . LBS.fromStrict)
  $ RSA.OAEP.decrypt Nothing (defaultOAEPParams SHA512) privateKey sbs

encrypt :: (Binary.Binary a) => PublicKey -> a -> IO (Maybe (Encrypted a))
encrypt (PublicKey publicKey) a =
  RSA.OAEP.encrypt (defaultOAEPParams SHA512)
                   publicKey
                   (LBS.toStrict $ Binary.encode a)
  >>= \case
    Left e -> pure Nothing
    Right encrypted -> pure (Just $ Encrypted encrypted)

sign :: (Binary.Binary a) => PrivateKey -> a -> IO (Maybe (Signed a))
sign p@(PrivateKey privateKey) a =
  RSA.PSS.sign Nothing
               (defaultPSSParams SHA512)
               privateKey
               (LBS.toStrict $ Binary.encode a)
  >>= \case
    Left e -> pure Nothing
    Right signature -> pure . Just $ Signed (publicOfPrivate p) signature a

verify :: (Binary.Binary a) => Signed a -> Maybe a
verify signed =
  RSA.PSS.verify (defaultPSSParams SHA512)  
                 (coerce $ signedBy signed)
                 (LBS.toStrict $ Binary.encode signed)
                 (signature signed)
  & \case
      True -> Just $ thing signed
      False -> Nothing

generateKeypair :: IO (PublicKey, PrivateKey)
generateKeypair = coerce <$> RSA.generate 2048 0x10001
