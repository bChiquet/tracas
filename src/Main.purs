module Main where

import Prelude (Unit, pure, (#), bind)
import Control.Monad ((>>=))
import Data.Functor (map)
import Data.String.Utils (words)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int as Int
import Data.Monoid ((<>))

import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Element (getAttribute)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)

type Url = String
type Milliseconds = Int

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

main :: Effect Unit
main = do
  mbConf <- buildConfig
  case mbConf of 
    Nothing   -> log "Couldn't build config (app name or server missing)"
    Just conf -> log ("Tracking " <> conf.appName) 
