-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Data.Route (Page(..), router, routeCodec) where

-- Language specifics
import Prelude hiding ((/))

-- Monad stuff
import Control.Alt ((<|>))

-- Data imports
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Number (fromString) as Number
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- Routing specifics
import Routing.Match (Match, lit)
import Routing.Duplex (RouteDuplex', root, segment, as, string, boolean)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- |All possible routes
data Page = Home          -- ^ The home page
          | Login         -- ^ The login page
          | Point String Boolean -- ^ The point management page, contains item key and readonly boolean
          | AddPoint Number Number -- ^The addpoint page, contains the lola of the point
          | Error                  -- ^ The Error page

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
num :: RouteDuplex' String -- ^String to parse
    -> RouteDuplex' Number -- ^The parsed number 
num = as show number
  where
    number :: String -> Either String Number
    number = maybe (Left "Number") Right <<< Number.fromString

-- |Bidirectional parsing and unparsing
routeCodec :: RouteDuplex' Page -- ^The router codec
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Error": "error" / noArgs
  , "Point": "point" / string segment / boolean segment
  , "AddPoint": "add" / num segment / num segment
  }