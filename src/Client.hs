{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Client where

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
  Just message <- (privateKey `writeTo` publicKey) contents
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
  Right a <- runClientM (send message) clientEnv
  pure a

receive' :: PrivateKey -> Integer -> IO [Message SBS.ByteString]
receive' privateKey daysBack = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
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
  (sub @"writes" $ arg @"message" @SBS.ByteString $ \message -> raw $ sendMessage message
  )
  <+>
  (sub @"receive" $ arg @"days-back" @Integer $ \t -> raw $ receiveMessages t
  )
  <+>
  (sub @"keygen" $ raw $ do
    (publicKey, privateKey) <- generateKeypair
    encodeFile "sam.private" privateKey
    encodeFile "sam.public" publicKey
  )
  where
    sendMessage :: SBS.ByteString -> IO ()
    sendMessage msg = do
      privateKey <- decodeFile "sam.private"
      publicKey <- decodeFile "sam.public"
      print =<< send' privateKey publicKey msg
    receiveMessages :: Integer -> IO ()
    receiveMessages daysBack = do
      privateKey <- decodeFile "sam.private"
      receive' privateKey daysBack >>= decryptAndDisplay privateKey
