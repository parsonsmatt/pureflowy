-- | TODO: This example could use a rewrite ;-)
module Main where

import Control.Monad.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import PureFlowy.Api
import Data.Argonaut.Generic.Aeson
import Data.Either
import Data.Generic
import Data.Maybe
import Prelude
import Servant.PureScript.Affjax
import Servant.PureScript.Settings
import PureFlowy.Api.MakeRequests as MakeReq
import Data.Array as Array
import Servant.Subscriber as Subscriber
import Servant.Subscriber.Connection as C
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
import Pux.Html (Html, text, button, span, div, p, h1, ul, li)
import Pux.Html.Events (onClick)
import Servant.Subscriber (Subscriber, makeSubscriber, SubscriberEff, Config, makeSubscriptions)
import Servant.Subscriber.Request (HttpRequest(..))
import Servant.Subscriber.Types (Path(Path))
import Signal (Signal)
import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)
import Unsafe.Coerce (unsafeCoerce)
import WebSocket (WEBSOCKET)

import PureFlowy.Api.Todos

type MySettings = SPSettings_ SPParams_

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL, err :: EXCEPTION  | eff)))

type ServantModel =
    { state :: State
    , effects :: Array (APIEffect () Action)
    }

data Action
    = Nop
    | ReportError AjaxError

type State =
    { todos :: Array Todo
    }

initialState :: State
initialState =
    { todos:
        [ Todo { todoDone: false, todoId: 1, todoItem: "wat" }
        ]
    }


update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update Nop = noEffects
update (ReportError _) = noEffects

view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text "PureFlowy" ]
    , ul [] (map mkTodo state.todos)
    ]
  where
    mkTodo (Todo { todoDone, todoItem } ) =
        li [] [ text todoItem ]

runEffect :: MySettings -> APIEffect () Action -> Aff (channel :: CHANNEL, ajax :: AJAX, err :: EXCEPTION) Action
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ ReportError err
      Right v -> pure v

type SubscriberData eff = {
  subscriber :: Subscriber eff Action
, messages :: Signal Action
}

-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL | e) Unit
main :: forall eff. Eff (ajax :: AJAX, err :: EXCEPTION, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff) Unit
main = do
  let settings = SPSettings_ {
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ { baseURL : "http://localhost:8081/" }
                  }
  app <- coerceEffects <<< start $
    { initialState: initialState
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html

coerceEffects :: forall eff0 eff1 a. Eff eff0 a -> Eff eff1 a
coerceEffects = unsafeCoerce
