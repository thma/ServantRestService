{-# LANGUAGE DataKinds         #-} -- Servant needs this to allow construction of api types
{-# LANGUAGE TypeOperators     #-} -- Servant needs this to allow operators like :> in types
{-# LANGUAGE OverloadedStrings #-} -- Servant.Swagger uses Text instead of STring
module SwaggerEntityService where

import EntityService (UserAPI, userAPI, userServer)
import Entities (User (..))
import Data.Swagger
import Servant.Swagger
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Lens
import Data.Aeson (toJSON)

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a User service with Swagger documentation.
type API = SwaggerAPI :<|> UserAPI

-- boilerplate to guide type inference
api :: Proxy API
api = Proxy

instance ToSchema User where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "This is the awesome User API (tm)"
      & mapped.schema.example ?~ toJSON (User "4711" "Max Muster" "mm@muster.com" )

-- | Swagger spec for user API.
swagger :: Swagger
swagger = toSwagger userAPI
    & host ?~ "127.0.0.1:8080" 
    & schemes ?~ [Http]
    & info.title   .~ "User API"
    & info.version .~ "1.23"
    & info.description ?~ "This is an API that tests swagger integration"
    & info.license ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | Combined server of a User service with Swagger documentation.
server :: Server API
server = return swagger :<|> userServer

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