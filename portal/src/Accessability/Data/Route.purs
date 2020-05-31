-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Data.Route (Page(..), router, routeCodec) where

-- Language specifics
import Prelude hiding ((/))
import Control.Alt ((<|>))

import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Number (fromString) as Number

-- Generics
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- Routing specifics
import Routing.Match (Match, lit)
import Routing.Duplex (RouteDuplex', root, segment, as, params)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- |All possible routes
data Page = Home  -- ^ The Home page
          | Login -- ^ The login page
          | Point String -- ^ The point management page, contains item key
          | AddPoint Number Number -- ^The addpoint page, contains the lola of the point
          | Error -- ^ The Error page

derive instance genericRoute :: Generic Page _
derive instance eqRoute :: Eq Page
derive instance ordRoute :: Ord Page
instance showPage :: Show Page where
  show = genericShow

-- | Routing function that creates data types based on the URL, we only deal with home and
-- login pages
router :: Match Page -- ^ The router
router = home <|> login
  where
    home = Home <$ lit ""
    login = Login <$ lit "login"


-- |Add a parser for number as a segment
num::RouteDuplex' String -> RouteDuplex' Number
num = as show number
  where
    number :: String -> Either String Number
    number = maybe (Left "Number") Right <<< Number.fromString

-- | Bidirectional parsing and unparsing
routeCodec :: RouteDuplex' Page -- ^ The router codec
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Error": "error" / noArgs
  , "Point": "point" / segment
  , "AddPoint": "add" / num segment / num segment
  }
