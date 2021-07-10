module Main where

import Prelude (Unit)


import Effect (Effect)
import Effect.Console (log)

type Url = String
type Milliseconds = Int

type Config =
  { appName :: String
  , dataset :: String
  , collectingServer :: Url
  , heartBeatFrequency :: Milliseconds
  , phoneHomeFrequency :: Milliseconds
  , recordedEvents :: Array String
  }

main :: Effect Unit
main = do
  log "Hello, world!"
