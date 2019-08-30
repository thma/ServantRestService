{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Entities where
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics
-- import SimplePersistence (Id, Entity, getId, persist, retrieve)
import JsonPersistence (Id, Entity, getId, persist, retrieve)

data User = User {
      userId :: Id
    , name   :: String
    , email  :: String
} deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Entity User where
    getId = userId

data Posting = Posting {
      postId  :: Id
    , userRef :: Id
    , text    :: String
} deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Entity Posting where
    getId = postId

retrieveUser :: Id -> IO User
retrieveUser = retrieve

retrievePosting :: Id -> IO Posting
retrievePosting = retrieve

-- little demo
main = do
    let user = User "1" "Heinz Meier" "hm@meier.com"
    let post = Posting "4711" "1" "My name is Heinz, this is my first post"

    persist user
    persist post

    user' <- retrieve "1" :: IO User
    user' <- retrieveUser "1"
    print user'

    retrievePosting "4711" >>= print
