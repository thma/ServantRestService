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

type UserAPI1 = "users" :> Get '[JSON] [User]
        :<|> "users" :> Capture "id" Id :> Get '[JSON] User
        :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ()

-- boilerplate to guide type inference
userAPI1  :: Proxy UserAPI1
userAPI1  = Proxy

server1 :: Server UserAPI1
server1 = 
    -- GET /users
    getAllUsers
    -- GET /users/:id
    :<|>  getUser
    -- POST /users/:id
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
postUser user = liftIO $ persist user

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server1

main :: IO ()
main = run 8080 app1    