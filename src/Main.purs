module Main where

import Prelude (Unit, pure, (#), bind, discard, unit)
import Control.Monad ((>>=), void)
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Functor (map)
import Data.String.Utils (words)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Number as Number
import Data.Traversable (traverse)
import Data.Time.Duration (Milliseconds(..))
import Data.Monoid ((<>))
import Data.Argonaut.Encode (encodeJson)

import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Element (getAttribute)
import Web.HTML (window)
import Web.HTML.Window (document, toEventTarget)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.Event.EventTarget
  (EventTarget, EventListener, addEventListener, eventListener)
import Web.Event.Event (Event, EventType(..))

import Effect.AVar as AVar
import Effect.Aff (Aff, launchAff_, delay)
import Affjax (post)
import Affjax.ResponseFormat as Format
import Affjax.RequestBody as Body

type Url = String
type DataStore = AVar.AVar Datum 

type Datum = Unit
type Report = Datum

type Config =
  { appName :: String
  , server :: Url
  , dataSet :: String
  , heartBeatEvery :: Milliseconds
  , phoneHomeEvery :: Milliseconds
  , recordedEvents :: Array String
  }

defaultEvts :: Array String
defaultEvts =
  ["click", "select", "mouseenter", "mouseleave"
  , "mousedown", "mouseup", "dragstart", "dragend"]

getAttr :: String -> Maybe Element -> Effect (Maybe String)
getAttr attr mbNode = case mbNode of
  Just node -> getAttribute attr node
  Nothing -> pure Nothing

withDefault :: forall a. a -> Maybe a -> a
withDefault = fromMaybe

buildConfig :: Effect (Maybe Config)
buildConfig = do
  tracasNode <-
        window 
    >>= document
    #   map toNonElementParentNode 
    >>= getElementById "tracas-include-script"

  mbAppName  <- getAttr "tracas-app-name" tracasNode
  mbServer   <- getAttr "tracas-server" tracasNode 
  dataSet    <- getAttr "tracas-data-set" tracasNode
  hBeatEvery <- getAttr "tracas-heartbeat-millis" tracasNode
              # map (\x -> x >>= Number.fromString # map Milliseconds)
  phoneEvery <- getAttr "tracas-phonehome-millis" tracasNode
              # map (\x -> x >>= Number.fromString # map Milliseconds)
  events     <- getAttr "tracas-needed-events" tracasNode
              # map (map words)

  case [mbAppName, mbServer] of
    [Just appName, Just server] -> pure (Just 
      { appName        : appName
      , server         : server
      , dataSet        : dataSet    # withDefault "undefined"
      , heartBeatEvery : hBeatEvery # withDefault (Milliseconds 200.0)
      , phoneHomeEvery : phoneEvery # withDefault (Milliseconds 5000.0)
      , recordedEvents : events     # withDefault defaultEvts})
    _ -> pure Nothing

onEvent :: DataStore -> Event -> Effect Unit
onEvent collector _event =
  void (AVar.put unit collector (\_ -> pure unit))

trackEvent :: EventTarget -> EventListener -> String -> Effect Unit
trackEvent target listener eventName =
  addEventListener (EventType eventName) listener true target

trackEvents :: Config -> DataStore -> Effect Unit
trackEvents config collector = do
  listener <- eventListener (onEvent collector)
  target <- window # map toEventTarget
  void (traverse (trackEvent target listener) config.recordedEvents)

sendData :: Config -> Array Report -> Aff Unit
sendData config data_ = do
  result <- post Format.json config.server (encodeJson data_ # Body.json # Just)
  case result of
    Left _ -> liftEffect (log "oops")
    Right _ -> pure unit

packData :: DataStore -> Aff (Array Datum)
packData _ = pure []

handleEvents :: Config -> DataStore -> Effect Unit
handleEvents config store = do
  launchAff_ (forever ( do
    packData store >>= sendData config 
    delay config.phoneHomeEvery
  ))

main :: Effect Unit
main = do
  mbConf <- buildConfig
  case mbConf of 
    Nothing   -> log "Couldn't build config (app name or server missing)"
    Just conf -> do
       log ("Tracking " <> conf.appName) 
       dataStore <- AVar.empty
       trackEvents conf dataStore
       handleEvents conf dataStore

