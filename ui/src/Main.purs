module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_(..))
import Data.Array as Array
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Pux (renderToDOM, start, EffModel, noEffects)
import Signal.Channel (CHANNEL)

import PureFlowy.Api (getTodos, postTodos, deleteTodosById, SPParams_(..))
import PureFlowy.Api.Todos (Todo(..))
import PureFlowy.State (State, initialState)
import PureFlowy.View (view)
import PureFlowy.Action (Action(..))
import PureFlowy.Settings (MySettings)

type App eff
    = ReaderT MySettings (ExceptT AjaxError (Aff (Effects eff)))

type Effects eff = (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | eff)

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update Nop state =
    noEffects state
update (ReportError _) state =
    noEffects state
update GetTodos state =
    runEffectActions state
        [ LoadTodos <$> getTodos "" -- Should be a Maybe
        ]
update (LoadTodos newTodos) state =
    noEffects state { todos = newTodos }
update (ModifyCurrentTodo ev) state =
    noEffects state { currentTodo = ev.target.value }
update CreateTodo state =
    runEffectActions (state { currentTodo = "" })
        [ do
            let todo =
                    { todoDone: false
                    , todoItem: state.currentTodo
                    , todoId: 0
                    }
            id <- postTodos (Todo todo)
            pure $ LoadTodos $ state.todos <> [Todo todo { todoId = id }]
        ]
update (DeleteTodo id) state =
    runEffectActions (state { todos = Array.filter p state.todos })
        [ Nop <$ deleteTodosById id
        ]
  where
    p (Todo r) = r.todoId /= id

runEffectActions :: State -> Array (App () Action) -> EffModel State Action (ajax :: AJAX)
runEffectActions state effects =
    { state: state
    , effects: map (runEffect state.settings) effects
    }

runEffect
    :: forall eff
     . MySettings
    -> App eff Action
    -> Aff (Effects eff) Action
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ ReportError err
      Right v -> pure v

main :: Eff (channel :: CHANNEL, err :: EXCEPTION, ajax :: AJAX) Unit
main = do
  let settings = SPSettings_ {
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ { baseURL : "http://localhost:8081/" }
                  }
  app <- start $
    { initialState: initialState settings
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
