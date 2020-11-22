-- |
-- | The authenticate interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.Authenticate
  ( UserInfo(..)
  , Authenticate(..)
  , class ManageAuthentication
  , login
  , logout) where

-- Language imports
import Prelude

-- Data imports
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.Argonaut
  (class DecodeJson
  , class EncodeJson
  , decodeJson
  , jsonEmptyObject
  , (.:)
  , (:=)
  , (~>))

-- Halogen imports
import Halogen (HalogenM, lift)

-- Import user types
import Accessibility.Interface.User (Role)

-- |The user information returned after an authenticate is successful
newtype UserInfo = UserInfo { userid     :: String -- ^The user identity key
                           , token    :: String -- ^The token used for the API
                           , username :: String -- ^The user name
                           , role     :: Role -- ^The role of the user
                           , email    :: String -- ^The email address of the user
                          }

-- We need to be able to unwrap it to make nice looking code ;-)
derive instance newtypeUserInfo :: Newtype UserInfo _

-- Show of the userinfo
instance showUserInfo :: Show UserInfo where
  show (UserInfo ui) = "UserInfo { userid=\"" <> ui.userid <> "\", token=\"" <> ui.token 
    <> "\"username=\"" <> ui.username <> "\", email=\"" <> ui.email <> "\"}"

-- JSON decoder for UserInfo
instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json = do
    obj ← decodeJson json
    userid ← obj .: "userid"
    token ← obj .: "token"
    username ← obj .: "username"
    role <- obj .: "role"
    email ← obj .: "email"
    pure $ UserInfo { userid, token, username, role, email }

-- |The authentication informaion needed to be able to authenticate the user and return a token
data Authenticate = Authenticate  { username :: String    -- ^The username
                                    , password :: String  -- ^The password
                                  }

instance encodeJsonPost :: EncodeJson Authenticate where
  encodeJson (Authenticate auth)
    = "username" := auth.username
    ~> "password" := auth.password
    ~> jsonEmptyObject
            
-- |The class for authentication
class Monad m <= ManageAuthentication m where

  -- |Tries to log in and returns with a token if succesful
  login :: Authenticate       -- ^Authentication information
        -> m (Maybe UserInfo) -- ^UserInfo
       
  -- |Logs out the user
  logout :: m Unit
  
-- |Avoid lift in the components
instance manageAuthenticationHalogenM :: ManageAuthentication m => ManageAuthentication (HalogenM st act slots msg m) where
  login = lift <<< login  
  logout = lift logout
