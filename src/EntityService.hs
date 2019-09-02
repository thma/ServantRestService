{-# LANGUAGE DataKinds       #-} -- Servant needs this to allow construction of api types
{-# LANGUAGE TypeOperators   #-} -- Servant needs this to allow operators like :> in types
--{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE DeriveDataTypeable         #-}
--{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityService where

import GHC.Generics
import Data.Aeson
import           Data.Swagger
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import           Servant.Swagger

import Entities
import JsonPersistence
import Control.Monad.IO.Class 
import           Control.Lens

type UserAPI = 
         "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "id" Id :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ()

-- boilerplate to guide type inference
userAPI  :: Proxy UserAPI
userAPI  = Proxy

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a User service with Swagger documentation.
type API = SwaggerAPI :<|> UserAPI

-- boilerplate to guide type inference
api :: Proxy API
api = Proxy

instance ToSchema User where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      -- & mapped.schema.description ?~ "This is some real User API right here"
      -- & mapped.schema.example ?~ toJSON (Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk")

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

-- | Swagger spec for user API.
userSwagger :: Swagger
userSwagger = toSwagger userAPI
--    & info.title   .~ "Todo API"
--    & info.version .~ "1.0"
--    & info.description ?~ "This is an API that tests swagger integration"
--    & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- | Combined server of a Todo service with Swagger documentation.
server :: Server API
server = return userSwagger :<|> userServer --error "not implemented"



-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "starting userAPI on port " ++ show port
    run port app    