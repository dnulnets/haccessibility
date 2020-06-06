-- |
-- | The Main module
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
import Data.Traversable (traverse)
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

-- | Routing imports
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)

-- Our own imports
import Accessibility.Utils.Token (readToken, removeToken, Token(..))
import Accessibility.Application (runApplication, Environment, default)
import Accessibility.Root as Root
import Accessibility.Interface.Endpoint (Endpoint(..), BaseURL(..), backend)
import Accessibility.Interface.Authenticate (UserInfo(..))
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
  log $ "Origin = " <> loc
  log $ "Build = " <> build

  HA.runHalogenAff do
    body <- HA.awaitBody

    -- Try to see if we have a token in local storage, then we can reuse that one
    -- if the backend approves it
    H.liftEffect readToken >>= traverse_ \(Token tok) -> do

      -- Ask the backend for approval by making a GET to the authenticate
      -- endpoint with the token as an Authroization: Bearer, it will reply
      -- userInfo if approved, otherwise an error
      res <- H.liftAff $ mkAuthRequest (BaseURL loc) Authenticate 
        (Just (UserInfo {
          token: tok
          , userid:""
          , email:""
          , username:""}))
        (Get::RequestMethod Void)
      
      case res of
        Left err -> do
          H.liftEffect $ log $ "Error: " <> err
          H.liftEffect $ log "No resuse of local storage token, did not get good response from backend"
          H.liftEffect $ removeToken
          H.liftEffect $ Ref.write Nothing cui
        Right (Tuple (AXS.StatusCode 200) userInfo) -> do
          H.liftEffect $ log $ "Reuse of local storage token"
          H.liftEffect $ log $ show userInfo
          H.liftEffect $ Ref.write userInfo cui
        Right (Tuple sc _) -> do
          H.liftEffect $ log $ "Error: " <> (show sc)
          H.liftEffect $ log "No resuse of local storage token, did not get good response from backend"
          H.liftEffect $ removeToken
          H.liftEffect $ Ref.write Nothing cui

    let
      env ∷ Environment
      env = (default cui) { baseURL = BaseURL loc }

    ui <- H.liftEffect $ Ref.read cui
    H.liftEffect $ log $ show ui
    io <- runUI (rootComponent env) ui body

    void $ liftEffect $ matchesWith (parse routeCodec) \old new -> do
      liftEffect $ log $ "Router at " <> show old
      liftEffect $ log $ "Router change to " <> show new
      when (old /= Just new) do
        launchAff_ $ io.query $ H.tell $ Root.GotoPageRequest new