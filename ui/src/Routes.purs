module App.Routes where

import Prelude

import Data.Functor
import Data.Maybe
import Control.Alt
import Pux.Router

data Route
    = Home
    | Todo String
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  (Todo <$> (lit "todo" *> str) <* end)
  <|>
  Home <$ end
