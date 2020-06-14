{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant
import Data.Time (UTCTime)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import GHC.TypeLits (Symbol)

import Messaging

type ServiceAPI = MessagingAPI '[ '("test" , SBS.ByteString) ]

type MessagingSubAPI ty =
  "send"
    :> ReqBody '[OctetStream] (Message ty)
    :> Post '[OctetStream] MessageResponse
  :<|> 
  "receive" 
    :> Capture "since" UTCTime
    :> Post '[OctetStream] [Message ty]

type family MessagingAPI (xs :: [(Symbol, *)]) :: * where
  MessagingAPI ('(name, ty) ': '[]) = name :> MessagingSubAPI ty
  MessagingAPI ('(name, ty) ': xs) = name :> MessagingSubAPI ty :<|> MessagingAPI xs
