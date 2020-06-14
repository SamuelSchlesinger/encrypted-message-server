{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Client where

import API (ServiceAPI)
import Messaging
import Options.Commander
import Servant.Client
import Data.Proxy
import Servant.API
import qualified Data.ByteString as SBS
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (Day(..))
import Data.Binary (decodeFile)
import Network.HTTP.Client (newManager, closeManager, defaultManagerSettings)
import Control.Exception (bracket)

send :<|> receive = client (Proxy @ServiceAPI)

send :: Message SBS.ByteString -> ClientM MessageResponse
receive :: UTCTime -> ClientM [Message SBS.ByteString]

main :: IO ()
main =  pure ()
  
  {- command_ . named @"client" $
  (sub @"writes" $ arg @"message" @String $ \message -> raw $ sendMessage message)
  <+>
  (sub @"receive" $ arg @"days-back" @Integer $ \t -> raw $ receiveMessages t)
  where
    sendMessage :: String -> IO ()
    sendMessage msg = bracket (newManager defaultManagerSettings) closeManager \manager -> do
      privateKey <- decodeFile "sam.private"
      publicKey <- decodeFile "sam.public"
      let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "/")
      print =<< runClientM (send _) clientEnv
    receiveMessages :: Integer -> IO ()
    receiveMessages daysBack = bracket (newManager defaultManagerSettings) closeManager \manager -> do
      privateKey <- decodeFile "sam.private"
      publicKey <- decodeFile "sam.public"
      let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "/")
      UTCTime (ModifiedJulianDay julianDay) _ <- getCurrentTime
      let fromThisTime = UTCTime (ModifiedJulianDay (julianDay - daysBack)) 0
      messages <- runClientM (receive fromThisTime) clientEnv
      pure ()
  -}
