-- |
-- | The REST request utilities functions
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Utils.Request(RequestMethod(..),
                          mkRequest,
                          mkRequest_,
                          mkAuthRequest,
                          mkAuthRequest_) where

-- Language imports
import Prelude

-- Data imports
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.HTTP.Method (Method(..))
import Data.Argonaut (
  Json
  , class DecodeJson
  , decodeJson
  , class EncodeJson
  , encodeJson)

-- Effects
import Effect.Aff (Aff)

-- Aff Rest imports
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.StatusCode as AXS

-- Routing imports
import Routing.Duplex (print)

--
-- Our own imports
--
import Accessibility.Interface.Endpoint (
  Endpoint,
  endpointCodec,
  BaseURL(..))
import Accessibility.Interface.Authenticate (UserInfo(..))

-- |type of Authorization 
newtype Authorization = Bearer String -- ^The constructor for "Authorization: Bearer <token>

-- |The request types we support
data RequestMethod a  = Get             -- ^GET request, make sure you set the type a as Void
                      | Post (Maybe a)  -- ^POST request
                      | Put (Maybe a)   -- ^PUT request
                      | Delete          -- ^DELETE request

-- |The base configuration for a REST call
defaultRequest  :: forall a. EncodeJson a
                => BaseURL              -- ^The URL 
                -> Endpoint             -- ^The endpoint on the URL
                -> RequestMethod a      -- ^Request method 
                -> Maybe Authorization  -- ^The authorization, if any
                -> AX.Request Json      -- ^The request
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
mkRequest :: forall a v. DecodeJson v
          => EncodeJson a
          => BaseURL           -- ^The base URL for the endpoint
          -> Endpoint          -- ^The endpoint to call            
          -> RequestMethod a   -- ^The request method to use for the call
          -> Aff (Either String (Tuple AXS.StatusCode v)) -- ^The result of the call
mkRequest burl ep rm = do
  response <- AX.request $ defaultRequest burl ep rm Nothing
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string out of affjax errors
    Right val -> (Tuple val.status) <$> (decodeJson val.body)

-- |Makes a request to the backend and return with status
mkRequest_  :: forall a v. DecodeJson v
            => EncodeJson a
            => BaseURL           -- ^The base URL for the endpoint
            -> Endpoint          -- ^The endpoint to call            
            -> RequestMethod a   -- ^The request method to use for the call
            -> Aff (Either String AXS.StatusCode) -- ^The result of the call
mkRequest_ burl ep rm = do
  response <- AX.request $ defaultRequest burl ep rm Nothing
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string gout of affjax errors
    Right val -> Right val.status

-- |Converts a UserInfo to an Authorization
mkAuthorization::UserInfo->Authorization
mkAuthorization (UserInfo t) = Bearer t.token

-- |Makes a request to the backend and return with status and result
mkAuthRequest :: forall a v. DecodeJson v
            => EncodeJson a
            => BaseURL          -- ^The base URL of the backend
            -> Endpoint         -- ^The endpoint at the backend
            -> Maybe UserInfo   -- ^Authorization information
            -> RequestMethod a  -- ^The request
            -> Aff (Either String (Tuple AXS.StatusCode v)) -- ^The result of the request
mkAuthRequest burl ep ui rm = do
  response <- AX.request $ defaultRequest burl ep rm $ mkAuthorization <$> ui
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string out of affjax errors
    Right val -> (Tuple val.status) <$> (decodeJson val.body)

-- |Makes a request to the backend and return with status and ignore result
mkAuthRequest_  :: forall a v. DecodeJson v
                => EncodeJson a
                => BaseURL          -- ^The base url of the backend
                -> Endpoint         -- ^The endpoint at the backend
                -> Maybe UserInfo   -- ^The authorization information
                -> RequestMethod a  -- ^The request method
                -> Aff (Either String AXS.StatusCode)  -- ^The result of the request
mkAuthRequest_ burl ep ui rm = do
  response <- AX.request $ defaultRequest burl ep rm $ mkAuthorization <$> ui
  pure case response of
    Left err -> Left $ AX.printError err -- Make a string gout of affjax errors
    Right val -> Right val.status