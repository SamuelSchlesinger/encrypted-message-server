{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Client where

import System.Environment (getEnv)
import qualified Data.Binary as Binary
import Data.Maybe (catMaybes)
import Control.Monad
import API (ServiceAPI)
import Messaging
import Options.Commander
import Servant.Client
import Data.Proxy
import Servant.API
import qualified Data.ByteString as SBS
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (Day(..))
import Data.Binary (decodeFile, encodeFile)
import Network.HTTP.Client (newManager, closeManager, defaultManagerSettings)
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS8

send :<|> receive = client (Proxy @ServiceAPI)

send :: Message SBS.ByteString -> ClientM MessageResponse
receive :: UTCTime -> ClientM [Message SBS.ByteString]

send' :: PrivateKey -> PublicKey -> SBS.ByteString -> IO MessageResponse
send' privateKey publicKey contents = do
  host <- getEnv "ENCRYPTED_SERVER_HOST"
  Just message <- (privateKey `writeTo` publicKey) contents
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host 8080 "")
  Right a <- runClientM (send message) clientEnv
  pure a

receive' :: PrivateKey -> Integer -> IO [Message SBS.ByteString]
receive' privateKey daysBack = do
  host <- getEnv "ENCRYPTED_SERVER_HOST"
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host 8080 "")
  UTCTime (ModifiedJulianDay julianDay) _ <- getCurrentTime
  let fromThisTime = UTCTime (ModifiedJulianDay (julianDay - daysBack)) 0
  Right a <- runClientM (receive fromThisTime) clientEnv
  pure a

decryptAndDisplay :: PrivateKey -> [Message SBS.ByteString] -> IO ()
decryptAndDisplay privateKey messages = do
  forM_ messages \msg -> do
    let msg' = readMessage privateKey msg
    maybe (pure ()) BS8.putStrLn $ msg'

main :: IO ()
main =  command_ . toplevel @"client" $
  (sub @"writes" $ arg @"message" @SBS.ByteString 
                 $ \message -> 
                 sub @"to"
                   $ arg @"homie" @SBS.ByteString $ \homie -> raw $ sendMessage homie message
  )
  <+>
  (sub @"receive" $ arg @"days-back" @Integer $ \t -> raw $ receiveMessages t
  )
  <+>
  (sub @"keygen" $ arg @"name" @SBS.ByteString $ \name -> raw $ do
    (publicKey, privateKey) <- generateKeypair
    encodeFile (BS8.unpack name <> ".private") privateKey
    encodeFile (BS8.unpack name <> ".public") publicKey
  )
  where
    sendMessage :: SBS.ByteString -> SBS.ByteString -> IO ()
    sendMessage homie msg = do
      privateKeyFile <- getEnv "PRIVATE_KEY"
      publicKeyFile <- getEnv "PUBLIC_KEY"
      homiePublicKey <- decodeFile (BS8.unpack homie)
      privateKey <- decodeFile privateKeyFile
      print =<< send' privateKey homiePublicKey msg
    receiveMessages :: Integer -> IO ()
    receiveMessages daysBack = do
      privateKeyFile <- getEnv "PRIVATE_KEY"
      privateKey <- decodeFile privateKeyFile
      receive' privateKey daysBack >>= decryptAndDisplay privateKey
