{-# LANGUAGE TypeApplications #-}

-- | This is the top level namespace for the API.
module PureFlowy.Api where

import Servant
import Network.Wai.Handler.Warp

type App = Raw

application :: Application
application = serve (Proxy @Raw) (serveDirectory "ui/assets")

main :: IO ()
main =
    run 8081 application

