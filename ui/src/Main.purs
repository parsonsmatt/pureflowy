module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Util (awaitBody, runHalogenAff)

import PureFlowy as App

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI App.ui App.initialState body
