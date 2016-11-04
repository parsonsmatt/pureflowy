-- | This is the top level namespace for the API.
module PureFlowy.Api where

import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import qualified PureFlowy.Api.Files as Files

type Api = Files.Endpoint

application :: Application
application = serve (Proxy :: Proxy Api) Files.handler

main :: IO ()
main = Warp.run 8081 application
