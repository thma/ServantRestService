{-# LANGUAGE DataKinds         #-} -- Servant needs this to allow construction of api types
{-# LANGUAGE TypeOperators     #-} -- Servant needs this to allow operators like :> in types
{-# LANGUAGE OverloadedStrings #-} -- Servant.Swagger uses Text instead of STring
module SwaggerEntityService where

import EntityService (UserAPI, userAPI, userServer)
import Entities (User (..))
import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Swagger.UI.Core -- the default implementation
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Lens
import Data.Aeson (toJSON)

-- | Swagger spec of Model type 'User'
instance ToSchema User where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "This is the awesome User API (tm)"
      & mapped.schema.example ?~ toJSON (User "4711" "Max Muster" "mm@muster.com" )

-- | Swagger spec for user API.
swaggerDoc :: Swagger
swaggerDoc = toSwagger userAPI
    & host ?~ "localhost:8080" 
    & schemes ?~ [Http]
    & info.title   .~ "User API"
    & info.version .~ "1.23"
    & info.description ?~ "This is an API that tests swagger integration"
    & info.license ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> UserAPI

-- | boilerplate to guide type inference
api :: Proxy API
api = Proxy

-- | Servant server for an API
server :: Server API
server = swaggerSchemaUIServer swaggerDoc
       :<|> userServer

app :: Application
app = serve api server

up :: IO ()
up = do
    let port = 8080
    putStrLn $ "starting userAPI on port " ++ show port
    run port app