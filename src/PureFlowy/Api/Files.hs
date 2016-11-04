-- | This module contains the logic and type involved in serving files from the
-- file system. The convention we're establishing is that the modules namespaced
-- under the API should expose an 'Endpoint' type and a 'handler' function.
-- These will be imported qualified.
module PureFlowy.Api.Files
    ( Endpoint
    , handler
    ) where

import Servant

type Endpoint = Raw

handler :: Application
handler = serveDirectory "ui/dist"

