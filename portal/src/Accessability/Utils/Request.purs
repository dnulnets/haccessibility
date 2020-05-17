-- |
-- | The REST request utilities functions
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Utils.Request(RequestMethod(..),
                          mkRequest,
                          mkRequest_,
                          mkAuthRequest,
                          mkAuthRequest_) where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Data.HTTP.Method (Method(..))

import Data.Argonaut (Json,
                      class DecodeJson,
                      decodeJson,
                      class EncodeJson,
                      encodeJson)

import Effect.Aff (Aff)

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.StatusCode as AXS

import Routing.Duplex (print)

--
-- Our own imports
--
import Accessability.Interface.Endpoint (
  Endpoint,
  endpointCodec,
  BaseURL(..))
import Accessability.Interface.Authenticate (UserInfo(..))

-- |type of Authorization 
newtype Authorization = Bearer String -- ^The constructor for "Authorization: Bearer <token>

-- |The request type
data RequestMethod a
  = Get
  | Post (Maybe a)
  | Put (Maybe a)
  | Delete

-- |The base configuration for a REST call
defaultRequest ∷ ∀ a. EncodeJson a ⇒ BaseURL 
               → Endpoint
               → RequestMethod a
               → Maybe Authorization
               → AX.Request Json
defaultRequest (BaseURL baseUrl) ep reqm auth =
  { method: Left method
  , url: baseUrl <> print endpointCodec ep
  , headers: case auth of
      Nothing -> []
      Just (Bearer t) -> [ AXRH.RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: AXRB.json <$> encodeJson <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: AXRF.json
  }
  where
    Tuple method body = case reqm of
      Get -> Tuple GET Nothing
      Post b -> Tuple POST b
      Put b -> Tuple PUT b
      Delete -> Tuple DELETE Nothing

-- |Makes a request to the backend and return with status and result
mkRequest ∷ ∀ a v. DecodeJson v
            ⇒ EncodeJson a
            ⇒ BaseURL           -- ^The base URL for the endpoint
            -> Endpoint          -- ^The endpoint to call            
            -> RequestMethod a   -- ^The request method to use for the call
            -> Aff (Either String (Tuple AXS.StatusCode v)) -- ^The result of the call
mkRequest burl ep rm = do
  response <- AX.request $ defaultRequest burl ep rm Nothing
  pure case response of
    Left err → Left $ AX.printError err -- Make a string out of affjax errors
    Right val → (Tuple val.status) <$> (decodeJson val.body)

-- |Makes a request to the backend and return with status
mkRequest_ ∷ ∀ a v. DecodeJson v
            ⇒ EncodeJson a
            ⇒ BaseURL
            -> Endpoint
            → RequestMethod a
            → Aff (Either String AXS.StatusCode)
mkRequest_ burl ep rm = do
  response <- AX.request $ defaultRequest burl ep rm Nothing
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string gout of affjax errors
    Right val -> Right val.status

-- |Converts a UserInfo to an Authorization
mkAuthorization::UserInfo->Authorization
mkAuthorization (UserInfo t) = Bearer t.token

-- |Makes a request to the backend and return with status and result
mkAuthRequest ∷ ∀ a v. DecodeJson v
            ⇒ EncodeJson a
            ⇒ BaseURL
            -> Endpoint
            -> Maybe UserInfo
            → RequestMethod a
            → Aff (Either String (Tuple AXS.StatusCode v))
mkAuthRequest burl ep ui rm = do
  response <- AX.request $ defaultRequest burl ep rm $ mkAuthorization <$> ui
  pure case response of
    Left err → Left $ AX.printError err -- Make a string out of affjax errors
    Right val → (Tuple val.status) <$> (decodeJson val.body)

-- |Makes a request to the backend and return with status and ignore result
mkAuthRequest_ ∷ ∀ a v. DecodeJson v
            ⇒ EncodeJson a
            ⇒ BaseURL
            -> Endpoint
            -> Maybe UserInfo
            → RequestMethod a
            → Aff (Either String AXS.StatusCode)
mkAuthRequest_ burl ep ui rm = do
  response <- AX.request $ defaultRequest burl ep rm $ mkAuthorization <$> ui
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string gout of affjax errors
    Right val -> Right val.status
