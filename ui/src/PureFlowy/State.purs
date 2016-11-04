module PureFlowy.State where

import Prelude

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

