{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (forM_)

import System.FilePath.TH (fileRelativeToAbsolute)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import Messaging
import Data.Binary (decodeFile)

main :: IO ()
main = do
  let keysPath = $(fileRelativeToAbsolute "keys")
  bobPublicKey <- decodeFile (keysPath <> "/bob.public")
  bobPrivateKey <- decodeFile (keysPath <> "/bob.private")
  alicePublicKey <- decodeFile (keysPath <> "/alice.public")
  alicePrivateKey <- decodeFile (keysPath <> "/alice.private")
  evePrivateKey <- decodeFile (keysPath <> "/eve.private")
  forM_ messages $ \x -> do

    Just encrypted <- encrypt bobPublicKey x
    let Just decrypted = decrypt bobPrivateKey encrypted
    if x == decrypted then pure () else error "Decrypting failed"

    Just signed <- sign bobPrivateKey encrypted
    let Just verified = verify signed
    if verified == encrypted then pure () else error "Verifying failed"

    Just msg <- (bobPrivateKey `writeTo` alicePublicKey) x
    let Nothing = readMessage evePrivateKey msg
    let Just decodedMsg = readMessage alicePrivateKey msg
    if x == decodedMsg then pure () else error "Decoding failed"

messages :: [SBS.ByteString]
messages =
  ["Hello, this is Sam",
   "Pow chicka wow wow",
   "141241241",
   "HELLO WORLD",
   "!!!!!>..",
   "YES, THIS IS A VERY IMPORTANT MESSAGE ONLY HEADED FOR THE MOST IMPORTANT \
   \PLACES. INDEED, WE CAN REALLY ENCRYPT LONG MESSAGES OVER HERE. HOW LONG? \
   \I WOULDN'T STICK AROUND TO FIND OUT. IT'S GONNA BE A LONG TIME TIL WE CAN'T \
   \SEND A MESSAGE SO LONG. Oh indeed! The messages that I can encode are longer \
   \than long. We can really get going on this, make a real long message. Get \
   \yourself to long-town, bruski.",
   "",
   BS8.pack $ take 1850 $ repeat ' ']
