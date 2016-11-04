-- | Welcome to PureFlowy! This is the source code for the Haskell backend of
-- the service. All modules directly related to PureFlowy will be namespaced
-- under here.
module PureFlowy where

import Servant

import qualified Network.Wai.Handler.Warp as Warp

type Api = Raw

application :: IO ()
application = Warp.run 8081 (serve (Proxy :: Proxy Raw) (serveDirectory "ui/assets"))
