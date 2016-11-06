{-# LANGUAGE TypeOperators #-}

-- | This is the top level namespace for the API.
module PureFlowy.Api where

import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import qualified PureFlowy.Api.Files as Files
import qualified PureFlowy.Api.Todos as Todos

type Api
    = Todos.Api
    :<|> Files.Endpoint

application :: Application
application = serve (Proxy :: Proxy Api) (Todos.handler :<|> Files.handler)

main :: IO ()
main = Warp.run 8081 application
