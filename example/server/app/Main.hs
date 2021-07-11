module Main where

import Lib (api)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = run 9000 (logStdoutDev api)
