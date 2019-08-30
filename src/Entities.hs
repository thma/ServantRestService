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

data Post = Post {
      postId  :: Id
    , userRef :: Id
    , text    :: String
} deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Entity Post where
    getId = postId

retrieveUser :: Id -> IO User
retrieveUser = retrieve

retrievePost :: Id -> IO Post
retrievePost = retrieve

-- little demo
main = do
    let user = User "1" "Heinz Meier" "hm@meier.com"
    let post = Post "4711" "1" "My name is Heinz, this is my first post"

    persist user
    persist post

    user' <- retrieve "1" :: IO User
    user' <- retrieveUser "1"
    print user'

    retrievePost "4711" >>= print
