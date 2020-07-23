-- |
-- | The Main module of the HAccessibility project
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- Get the build constant
import Version (build)

-- Standard imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Data.String.CodeUnits as Str
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- Effect imports
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Console (log)

-- Affjax things
import Affjax.StatusCode as AXS

-- Halogen imports
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- Routing
import Routing.Hash (setHash, matchesWith)
import Routing.Duplex (parse, print)

-- Web imports
import Web.HTML (window)
import Web.HTML.Window (location, toEventTarget)
import Web.HTML.Location (origin)
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.Event.EventTarget (eventListener, addEventListener)

-- Control routines
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA

-- Our own imports
import Accessibility.Utils.Token (readToken, removeToken, Token(..))
import Accessibility.Application (runApplication, Environment, default)
import Accessibility.Root as Root
import Accessibility.Interface.Endpoint (Endpoint(..), BaseURL(..))
import Accessibility.Data.Route (routeCodec, Page(..))
import Accessibility.Utils.Request (RequestMethod(..), mkAuthRequest)

-- | Produce events from the browser for route changes
hashChangeProducer ∷ CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener ← eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    window
      >>= toEventTarget
      >>> addEventListener HCET.hashchange listener false

-- | Handle the change message and parse the URL to extract the page and page paramters and send it to
-- | the root page
hashChangeConsumer ∷ (∀ a. Root.Query a -> Aff (Maybe a)) → CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
      result = parse routeCodec hash
      newPage = case result of
        Left _ -> Home
        Right page -> page
  liftEffect $ log $ "New URL = '" <> hash <> "'," <> show result
  void $ query $ H.tell $ Root.GotoPageRequest newPage
  pure Nothing

-- | Hoist in our Application monad
rootComponent ∷ Environment →                -- ^ The Environment
                H.Component HH.HTML Root.Query Root.Input Void Aff    -- ^ The Application root component
rootComponent env = H.hoist (runApplication env) Root.component

-- | The main entry point for our application
main ∷ Effect Unit -- ^ Default return value
main = do
  cui <- liftEffect $ Ref.new Nothing
  loc <- window >>= location >>= origin
  log $ "Origin: " <> loc
  log $ "Build: " <> build

  HA.runHalogenAff do
    body <- HA.awaitBody

    -- Try to see if we have a token in local storage, then we can reuse that one
    -- if the backend approves it
    H.liftEffect readToken >>= traverse_ \(Token tok) -> do

      -- Ask the backend for approval by making a GET to the authenticate
      -- endpoint with the token as an Authorization: Bearer, it will reply
      -- with 200OK and userInfo if approved, everything else is considered
      -- denied
      res <- H.liftAff $ mkAuthRequest (BaseURL loc) Authenticate 
        (Just tok)
        (Get::RequestMethod Void)
      
      case res of
        Left err -> 
          H.liftEffect do
            log $ "Error: " <> err
            log "No resuse of local storage token, did not get good response from backend"
            removeToken
            Ref.write Nothing cui
            setHash $ print routeCodec Login

        Right (Tuple (AXS.StatusCode 200) userInfo) -> do
          H.liftEffect do
            log $ "Reuse of local storage token"
            log $ "User is: " <> (show userInfo)
            Ref.write userInfo cui
            setHash $ print routeCodec Home

        Right (Tuple sc _) -> do
          H.liftEffect do 
            log $ "Error responsecode: " <> (show sc)
            log "No resuse of local storage token, did not get good response from backend"
            removeToken
            Ref.write Nothing cui
            setHash $ print routeCodec Login

    -- Get the authenticated user if we already have one
    ui <- H.liftEffect $ Ref.read cui

    -- Run the root component
    let
      env ∷ Environment
      env = (default cui) { baseURL = BaseURL loc }
    io <- runUI (rootComponent env) ui body

    -- Our router, based on what is typed in the browser navigational feed
    void $ liftEffect $ matchesWith (parse routeCodec) \old new -> do
      when (old /= Just new) do
        launchAff_ $ io.query $ H.tell $ Root.GotoPageRequest new