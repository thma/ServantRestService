{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Lib where

import Data.Time.Calendar
import Prelude.Compat

import GHC.Generics
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
type UserAPI1 = "users" :> Get '[JSON] [User]

-- boilerplate to guide type inference
userAPI1  :: Proxy UserAPI1
userAPI1  = Proxy

server1 :: Server UserAPI1
server1 = return users1

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server1

startApp1 :: IO ()
startApp1 = run 8080 app1

-- 2nd api
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

userAPI2  :: Proxy UserAPI2
userAPI2  = Proxy
           

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac

app2 :: Application
app2 = serve userAPI2 server2

startApp2 :: IO ()
startApp2 = run 8080 app2
     
-- example 3
type UserAPI3 = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email


userApi3 :: Proxy UserAPI3
userApi3 = Proxy

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                -- ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"


server3 :: Server UserAPI3
server3 = position
      :<|> hello
      :<|> marketing

  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)

app3 :: Application
app3 = serve userApi3 server3

startApp3 :: IO ()
startApp3 = run 8080 app3
                
