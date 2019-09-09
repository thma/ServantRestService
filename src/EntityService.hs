{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
module EntityService where

import           Control.Exception          hiding (Handler)
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Typeable              (typeRep)
import           Description                (Desc)
import           Entities
import           GHC.Generics
import           JsonPersistence
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant


-- | REST api for User Entities
type UserAPI =
       "users" :> Summary "retrieve all users"
               :> QueryParam' '[Optional, Desc Int "max number of records to load"] "maxRecords" Int
               :> Get '[ JSON] [User]
  :<|> "users" :> Summary "retrieve user identified by :id"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> Get  '[ JSON] User
  :<|> "users" :> Summary "store a new user"
               :> ReqBody '[ JSON] User
               :> Post '[ JSON] ()
  :<|> "users" :> Summary "update existing user"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> ReqBody '[ JSON] User
               :> Put '[ JSON] ()
  :<|> "users" :> Summary "delete existing user"
               :> Capture' '[Desc Id "unique identifier"] ":id" Id
               :> Delete '[ JSON] ()

-- | implements the UserAPI
userServer :: Server UserAPI
userServer =
        getAllUsers   -- GET /users
  :<|>  getUser       -- GET /users/{id}
  :<|>  postUser      -- POST /users
  :<|>  putUser       -- POST /users/{id}
  :<|>  deleteUser    -- DELETE /users/{id}

-- | handler functions
getAllUsers :: Maybe Int -> Handler [User]
getAllUsers max = do
  liftIO $ putStrLn "GET /users"
  eitherUsersEx <- liftIO $ try (retrieveAll max) :: Handler (Either PersistenceException [User])
  case eitherUsersEx of
    Left ex -> throwAsServerError ex
    Right l -> return l

getUser :: Id -> Handler User
getUser id = do
  liftIO $ putStrLn $ "GET /users/" ++ id
  eitherUserEx <- liftIO $ try (retrieve id) :: Handler (Either PersistenceException User)
  case eitherUserEx of
    Left ex -> throwAsServerError ex
    Right u -> return u

postUser :: User -> Handler ()
postUser user = do
  liftIO $ putStrLn $ "POST /users/ " ++ show user
  eitherVoidEx <- liftIO $ try (liftIO $ post user) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v


putUser :: Id -> User -> Handler ()
putUser id user = do
  liftIO $ putStrLn $ "PUT /users/" ++ id ++ " " ++ show user
  eitherVoidEx <- liftIO $ try (put id user) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v

deleteUser :: Id -> Handler ()
deleteUser id = do
  liftIO $ putStrLn $ "DELETE /users/" ++ id
  liftIO $ delete userType id
  where
    userType = Proxy :: Proxy User


-- | throw a persistence exception as a Servant ServerError
throwAsServerError:: MonadError ServerError m => PersistenceException -> m a
throwAsServerError ex =
  throwError $
    case ex of
      EntityNotFound msg      -> err404 {errBody = pack msg}
      EntityAlreadyExists msg -> err409 {errBody = pack msg}
      InternalError msg       -> err500 {errBody = pack msg}


-- | boilerplate to guide type inference
userAPI :: Proxy UserAPI
userAPI = Proxy

-- the following is not actually needed, it just exists for local testing
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI userServer

demo :: IO ()
demo = do
  let port = 8080
  putStrLn $ "starting userAPI on port " ++ show port
  run port app

