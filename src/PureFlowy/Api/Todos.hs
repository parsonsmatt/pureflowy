{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

-- | This module describes the todo API in all of it's simplistic glory. This
-- follows the 'Api' and 'handler' pattern establisehd in the Files part.
module PureFlowy.Api.Todos where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Foldable          (find)
import           Data.IORef             (IORef, modifyIORef', newIORef,
                                         readIORef)
import           GHC.Generics           (Generic)
import           Servant
import           System.IO.Unsafe       (unsafePerformIO)

-- | The type of the API is namespaced under "todos" and we delegate to
-- a further type 'TodoCrud' for the actual handling and routes.
type Api =
    "todos" :> TodoCRUD

-- | This specifies four endpoints:
--
-- * At @GET todos@, get a list of the todos.
-- * At @GET todos/:id@, get a todo by id.
-- * At @POST todos@, create a 'Todo' and return the ID.
-- * At @DELETE todos/:id@, delete the todo with the given ID.
type TodoCRUD
    = Get '[JSON] [Todo]
    :<|> Capture "id" Int :> Get '[JSON] Todo
    :<|> ReqBody '[JSON] Todo :> Post '[JSON] Int
    :<|> Capture "id" Int :> Delete '[JSON] ()

data Todo = Todo
    { todoDone :: Bool
    , todoId   :: Int
    , todoItem :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The definition of the handlers lines up exactly with the definition of the
-- type.
handler :: Server Api
handler = listTodos
    :<|> getTodo
    :<|> createTodo
    :<|> deleteTodo

-- | To list all the available 'Todo', we read the 'IORef' in memory database of
-- 'Todo's.
listTodos :: Handler [Todo]
listTodos = liftIO (readIORef todoDatabase)

-- | Getting a single 'Todo' by the 'Int' identifier reads rather naturally.
-- First, we reuse the 'listTodos' handler function, returning the list of
-- 'Todo's. Then, we use the 'find' function to find the 'Todo' with the
-- matching ID. If that comes up with 'Nothing', then we throw a 404 error.
-- Otherwise, we return the matching todo item.
getTodo :: Int -> Handler Todo
getTodo i = do
    todos <- listTodos
    case find (\todo -> i == todoId todo) todos of
        Nothing ->
            throwError err404
        Just todo ->
            pure todo

-- | When creating a 'Todo', we ignore the ID, and instead give it the next ID
-- in the list.
createTodo :: Todo -> Handler Int
createTodo postedTodo = do
    todos <- listTodos
    let newId = 1 + maximum (map todoId todos)
        todo = postedTodo { todoId = newId }
    liftIO (modifyIORef' todoDatabase (todo :))
    pure newId

-- | When deleting a 'Todo', we just filter the todo lists, keeping only todos
-- that do not have the same todo ID.
deleteTodo :: Int -> Handler ()
deleteTodo i =
    liftIO (modifyIORef' todoDatabase (filter (\todo -> i /= todoId todo)))

-- | This is the "global mutable reference" pattern. Generally, this is a really
-- bad idea: you'll want to pass things as parameters, not refer to them like
-- this.
todoDatabase :: IORef [Todo]
todoDatabase = unsafePerformIO . newIORef $
    zipWith
        (Todo False)
        [1..]
        ["Wash the cats.", "Save the trees."]
{-# NOINLINE todoDatabase #-}
