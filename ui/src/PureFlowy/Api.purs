-- File auto generated by servant-purescript! --
module PureFlowy.Api where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable(), toNullable)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit)
import Prim (Array, Int, String)
import PureFlowy.Api.Todos (Todo)
import Servant.PureScript.Affjax (AjaxError(..), affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettings_(..), gDefaultToURLPiece)
import Servant.PureScript.Util (encodeHeader, encodeListQuery, encodeQueryItem, encodeURLPiece, getResult)

newtype SPParams_ = SPParams_ { baseURL :: String
                              }

getTodos :: forall eff m.
            (MonadReader (SPSettings_ SPParams_) m, MonadError AjaxError m, MonadAff ( ajax :: AJAX | eff) m)
            => String -> m (Array Todo)
getTodos sortBy = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqUrl = baseURL <> "todos" 
        <> "?" <> encodeQueryItem spOpts_' "sortBy" sortBy
  let reqHeaders =
        []
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 }
  affResp <- affjax affReq
  getResult affReq decodeJson affResp
  
getTodosById :: forall eff m.
                (MonadReader (SPSettings_ SPParams_) m, MonadError AjaxError m, MonadAff ( ajax :: AJAX | eff) m)
                => Int -> m Todo
getTodosById id = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqUrl = baseURL <> "todos" <> "/" <> encodeURLPiece spOpts_' id
  let reqHeaders =
        []
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 }
  affResp <- affjax affReq
  getResult affReq decodeJson affResp
  
postTodos :: forall eff m.
             (MonadReader (SPSettings_ SPParams_) m, MonadError AjaxError m, MonadAff ( ajax :: AJAX | eff) m)
             => Todo -> m Int
postTodos reqBody = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let reqUrl = baseURL <> "todos"
  let reqHeaders =
        []
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 , content = toNullable <<< Just <<< printJson <<< encodeJson $ reqBody
                 }
  affResp <- affjax affReq
  getResult affReq decodeJson affResp
  
deleteTodosById :: forall eff m.
                   (MonadReader (SPSettings_ SPParams_) m, MonadError AjaxError m, MonadAff ( ajax :: AJAX | eff) m)
                   => Int -> m Unit
deleteTodosById id = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "DELETE"
  let reqUrl = baseURL <> "todos" <> "/" <> encodeURLPiece spOpts_' id
  let reqHeaders =
        []
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 }
  affResp <- affjax affReq
  getResult affReq decodeJson affResp
  