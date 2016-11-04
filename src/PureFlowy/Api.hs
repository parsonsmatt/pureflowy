-- | This is the top level namespace for the API.
module PureFlowy.Api where

import qualified Network.Wai.Handler.Warp as Warp
import           Servant

type Api = Raw

application :: Application
application = serve (Proxy :: Proxy Api) (serveDirectory "ui/dist")

main :: IO ()
main =
    Warp.run 8081 application
