module Main where

import Prelude (Unit, pure, (#), bind, discard, unit)
import Control.Monad ((>>=), void)
import Data.Functor (map)
import Data.String.Utils (words)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int as Int
import Data.Traversable (traverse)
import Data.Monoid ((<>))

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

type Url = String
type Milliseconds = Int
type DataStore = AVar.AVar Datum 

type Datum = Unit

type Config =
  { appName :: String
  , collectingServer :: Url
  , dataSet :: String
  , heartBeatFrequency :: Milliseconds
  , phoneHomeFrequency :: Milliseconds
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

  mbAppName <- getAttr "tracas-app-name" tracasNode
  mbServer  <- getAttr "tracas-server" tracasNode 
  dataSet   <- getAttr "tracas-data-set" tracasNode
  hBeatFreq <- getAttr "tracas-heartbeat-millis" tracasNode
             # map (\x -> x >>= Int.fromString)
  phoneFreq <- getAttr "tracas-phonehome-millis" tracasNode
             # map (\x -> x >>= Int.fromString)
  events    <- getAttr "tracas-needed-events" tracasNode
             # map (map words)

  case [mbAppName, mbServer] of
    [Just appName, Just server] -> pure (Just 
      { appName            : appName
      , collectingServer   : server
      , dataSet            : dataSet   # withDefault "undefined"
      , heartBeatFrequency : hBeatFreq # withDefault 200
      , phoneHomeFrequency : phoneFreq # withDefault 5000
      , recordedEvents     : events    # withDefault defaultEvts})
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

main :: Effect Unit
main = do
  mbConf <- buildConfig
  case mbConf of 
    Nothing   -> log "Couldn't build config (app name or server missing)"
    Just conf -> do
       log ("Tracking " <> conf.appName) 
       dataStore <- AVar.empty
       trackEvents conf dataStore
--       handleEvents conf dataStore

