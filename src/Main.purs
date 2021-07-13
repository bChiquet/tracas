module Main where

import Prelude (Unit, pure, (#), bind, discard, unit, (-), (<), (*))
import Control.Monad ((>>=), void)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Loops (whileJust)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Data.Functor (map)
import Data.Array ((:), length)
import Data.String.Utils (words)
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Number as Number
import Data.Int as Int
import Math (floor)
import Data.EuclideanRing (div)
import Data.Traversable (traverse)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)
import Data.Monoid ((<>))
import Data.Argonaut.Encode (encodeJson)
import Browser.Cookie (getCookie, setCookie)
import Browser.Cookies.Data
  (Cookie(..), SetCookie(..), CookieOpts(..), SameSite(..))
import Effect.Now (now)
import Data.Unfoldable (replicateA)

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
import Effect.Aff.AVar as AffV
import Effect.Aff (Aff, launchAff_, delay)
import Affjax (post, printError)
import Affjax.ResponseFormat as Format
import Affjax.RequestBody as Body

type Url = String
type Identifier = String
type DataStore = AVar.AVar Datum 

type Datum = Unit
type Report = Datum

type Config =
  { appName :: String
  , server :: Url
  , session :: Identifier
  , cookie :: Identifier
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


-- | gets successive remainders of an int divided by 64 until it reaches 0.
-- | Operates on `Number` because `Int` is limited to 2^32
-- | Precision is neglected, since this is used for a hash.
successiveModulos :: Number -> Array Int
successiveModulos n = 
  let intDiv m divider = floor (div m divider)
      mod m divider = m - (intDiv m divider * divider) 
      allMods n_ modList = case intDiv n_ 64.0 of
        epsilon | epsilon < 1.0   -> mod n_ 64.0 : modList
        rest                      -> allMods rest ((mod n_ 64.0) : modList) 
   in map Int.floor (allMods n [])

-- | create an identifier based on creation time and random input
makeIdentifier :: Effect String
makeIdentifier = do
  let charSet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@#" 
  (Milliseconds posix) <- now # map unInstant
  let prefix = successiveModulos posix
  suffix <- replicateA (16 - length prefix) (randomInt 0 63)
  map (\n -> charAt n charSet # withDefault '?') (prefix <> suffix)
    # fromCharArray
    # pure

-- | Get cookie from key.
-- | If no cookie is retrieved, store one with defaultValue.
getOrCreateCookie :: String -> Identifier -> Effect Identifier
getOrCreateCookie cookieKey defaultValue = do
  mbCookie <- getCookie cookieKey
  cookieValue <- case mbCookie of
    Just (Cookie content) ->
      pure content.value
    Nothing     -> do
      setCookie 
        (SetCookie 
          {cookie: (Cookie {key: cookieKey, value: defaultValue})
          , opts: Just (CookieOpts 
             { domain : Nothing
             , expires : Nothing
             , httpOnly : false
             , maxAge : Nothing
             , path : Nothing
             , samesite : Just Lax
             , secure : true
             })
          }
        )
      pure defaultValue
  pure cookieValue

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

  session <- makeIdentifier
  cookie <- getOrCreateCookie "tracas-cookie" session

  case [mbAppName, mbServer] of
    [Just appName, Just server] -> pure (Just 
      { appName        : appName
      , server         : server
      , session        : session 
      , cookie         : cookie
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
    Left error -> liftEffect (log (printError error))
    Right _ -> pure unit

packData :: DataStore -> Aff (Array Datum)
packData store = 
  whileJust (AffV.tryTake store) pure

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

