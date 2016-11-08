module PureFlowy.View where

import Prelude

import Pux.Html.Events (onClick)
import Pux.Html (Html, text, button, span, div, p, h1, ul, li)

import PureFlowy.Action
import PureFlowy.State
import PureFlowy.Api.Todos

view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text "PureFlowy" ]
    , ul [] (map mkTodo state.todos)
    , button [onClick \_ -> GetTodos]
        [ text "Get Todos"
        ]
    ]
  where
    mkTodo (Todo { todoDone, todoItem }) =
        li [] [ text todoItem ]
