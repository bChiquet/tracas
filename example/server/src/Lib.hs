{-# language TypeOperators, DataKinds #-}
module Lib
    ( api
    ) where

import Servant.API (Raw, Post, ReqBody, JSON, (:>), (:<|>)(..))
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Servant.Server (serve)
import Data.Proxy (Proxy(Proxy))
import Data.Aeson (Value)

type Api 
  = "reports" :> ReqBody '[JSON] Value :> Post '[JSON] () 
  :<|> Raw -- serve files from parent directory

appServer
  = (\body -> pure ())
  :<|> serveDirectoryWebApp "../"

api = serve (Proxy :: Proxy Api) appServer
