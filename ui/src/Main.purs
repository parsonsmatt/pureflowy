module Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import PureFlowy.Api
import Data.Argonaut.Generic.Aeson
import Data.Either
import Data.Generic
import Data.Maybe
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import PureFlowy.Api.MakeRequests as MakeReq
import Data.Array as Array
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM.Node.Node (baseURI)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Pux (renderToDOM, fromSimple, start, EffModel, noEffects)
import Signal (Signal)
import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)
import Unsafe.Coerce (unsafeCoerce)
import WebSocket (WEBSOCKET)

import PureFlowy.Api as Api
import PureFlowy.Api.Todos
import PureFlowy.State
import PureFlowy.View
import PureFlowy.Action
import PureFlowy.Settings

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
        [ LoadTodos <$> Api.getTodos "" -- Should be a Maybe
        ]
update (LoadTodos newTodos) state =
    noEffects state { todos = newTodos }
update (ModifyCurrentTodo ev) state =
    noEffects state { currentTodo =  ev.target.value }
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
    p (Todo { todoId }) = todoId /= id




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

main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION, ref :: REF | eff) Unit
main = do
  let settings = SPSettings_ {
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ { baseURL : "http://localhost:8081/" }
                  }
  app <- coerceEffects <<< start $
    { initialState: initialState settings
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html

coerceEffects :: forall eff0 eff1 a. Eff eff0 a -> Eff eff1 a
coerceEffects = unsafeCoerce
