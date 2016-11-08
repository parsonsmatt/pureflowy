module PureFlowy.State where

import Prelude

import PureFlowy.Api
import PureFlowy.Api.Todos
import PureFlowy.Settings

type State =
    { todos :: Array Todo
    , settings :: MySettings
    }

initialState :: MySettings -> State
initialState s =
    { todos:
        [ Todo { todoDone: false, todoId: 1, todoItem: "wat" }
        ]
    , settings: s
    }
