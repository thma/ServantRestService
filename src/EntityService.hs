{-# LANGUAGE DataKinds       #-} -- Servant needs this to allow construction of api types
{-# LANGUAGE TypeOperators   #-} -- Servant needs this to allow operators like :> in types
module EntityService where

import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Entities
import JsonPersistence
import Control.Monad.IO.Class 

type UserAPI = 
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "id" Id :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ()

-- boilerplate to guide type inference
userAPI  :: Proxy UserAPI
userAPI  = Proxy

userServer :: Server UserAPI
userServer = 
    -- GET /users
    getAllUsers
    -- GET /users/:id
    :<|>  getUser
    -- POST /users
    :<|>  postUser

getAllUsers :: Handler [User]
getAllUsers = do
    liftIO $ putStrLn "GET /users"
    liftIO retrieveAll

getUser :: Id -> Handler User
getUser id = do
    liftIO $ putStrLn $ "GET /users/" ++ id
    liftIO $ retrieve id

postUser :: User -> Handler ()
postUser user = do
    liftIO $ putStrLn $ "POST /users/ " ++ show user
    liftIO $ persist user


-- the following is not actually needed, it just exists for local testing

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI userServer

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "starting userAPI on port " ++ show port
    run port app    