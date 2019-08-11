{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    , app1
    ) where

import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

type UserAPI1 = "users" :> Get '[JSON] [User]

-- boilerplate to guide type inference
userAPI1  :: Proxy UserAPI1
userAPI1  = Proxy

server1 :: Server UserAPI1
server1 = return users1

app1 :: Application
app1 = serve userAPI1 server1

startApp :: IO ()
startApp = run 8080 app1
