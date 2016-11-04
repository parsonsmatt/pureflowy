module PureFlowy 
  ( module PureFlowy.State
  , module PureFlowy
  ) where

import Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH

import PureFlowy.State (State, initialState)

data Query a = ToggleState a

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Don't push me"
              else "I said don't push me!"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query g
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    pure next
