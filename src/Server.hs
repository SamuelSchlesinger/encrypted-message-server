{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Server where

import Messaging
import API

import qualified Data.Binary as Binary
import Servant
import qualified Data.ByteString as SBS
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (getEnv)

server
  :: PrivateKey
  -> PublicKey
  -> TVar [(UTCTime, Message SBS.ByteString)]
  -> Server ServiceAPI
server privateKey publicKey t = recordMessage :<|> retrieveMessages where
  retrieveMessages lastTime = do
    messages <- liftIO $ atomically (readTVar t)
    pure (map snd . filter ((>= lastTime) . fst) $ messages)
  recordMessage message = do
    now <- liftIO $ getCurrentTime
    liftIO $ atomically do
      messages <- readTVar t
      writeTVar t ((now, message) : messages)
      pure $ MessageResponse { serverPublicKey = publicKey }

main :: IO ()
main = do
  publicKeyFile <- getEnv "PUBLIC_KEY"
  privateKeyFile <- getEnv "PRIVATE_KEY"
  publicKey <- Binary.decodeFile publicKeyFile
  privateKey <- Binary.decodeFile privateKeyFile
  t <- newTVarIO []
  run 8080 (logStdoutDev $ serve (Proxy @ServiceAPI) $ server privateKey publicKey t)
