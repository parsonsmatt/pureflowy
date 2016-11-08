module PureFlowy.Action where

import Prelude

import Servant.PureScript.Affjax
import PureFlowy.Api.Todos

data Action
    = Nop
    | ReportError AjaxError
    | GetTodos
    | LoadTodos (Array Todo)

