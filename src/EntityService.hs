{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
module EntityService where

import           Control.Monad.IO.Class
import           Control.Exception hiding (Handler)
import           Data.Aeson
import           Description              (Desc)
import           Entities
import           GHC.Generics
import           JsonPersistence
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Data.Typeable (typeRep)
import Data.Maybe (fromMaybe)

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
  liftIO (retrieveAll max)

getUser :: Id -> Handler User
getUser id = do
  liftIO $ putStrLn $ "GET /users/" ++ id
  eitherUserEx <- liftIO $ try (retrieve id) :: Handler (Either PersistenceException User)
  case eitherUserEx of
    Left ex -> throwError $ mapToServerError ex
    Right u -> return u

mapToServerError :: PersistenceException -> ServerError
mapToServerError EntityNotFound      = err404 {errBody = "nothing here"}
mapToServerError InternalError       = err500
mapToServerError EntityAlreadyExists = err409

postUser :: User -> Handler ()
postUser user = do
  liftIO $ putStrLn $ "POST /users/ " ++ show user
  liftIO $ persist user

putUser :: Id -> User -> Handler ()
putUser id user = do
  liftIO $ putStrLn $ "put /users/" ++ id ++ " " ++ show user
  liftIO $ persist user
  
deleteUser :: Id -> Handler ()
deleteUser id = do
  liftIO $ putStrLn $ "DELETE /users/" ++ id
  liftIO $ delete userType id
  where
    userType = Proxy :: Proxy User

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

