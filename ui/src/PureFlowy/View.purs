module PureFlowy.View where

import Prelude hiding (div)

import Pux.Html.Attributes hiding (form)
import Pux.Html.Events
import Pux.Html hiding (map)

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
    , form
        [ name "updatetodo"
        , onSubmit \_ -> CreateTodo
        ]
        [ input
            [ type_ "text"
            , value state.currentTodo
            , onChange ModifyCurrentTodo
            ]
            []
        ]
    ]
  where
    mkTodo (Todo { todoDone, todoItem, todoId }) =
        li []
            [ button
                [ onClick \_ -> DeleteTodo todoId ]
                [ text "X" ]
            , text todoItem
            ]
