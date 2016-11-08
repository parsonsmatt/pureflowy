module PureFlowy.Action where

import Prelude

import Servant.PureScript.Affjax
import PureFlowy.Api.Todos
import Pux.Html.Events

data Action
    = Nop
    | ReportError AjaxError
    | GetTodos
    | LoadTodos (Array Todo)
    | ModifyCurrentTodo FormEvent
    | CreateTodo
    | DeleteTodo Int
