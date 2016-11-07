-- File auto generated by purescript-bridge! --
module PureFlowy.Api.Todos where

import Prim (Boolean, Int, String)

import Data.Generic (class Generic)


data Todo =
    Todo {
      todoDone :: Boolean
    , todoId :: Int
    , todoItem :: String
    }

derive instance genericTodo :: Generic Todo

