{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Strict #-}
module JsonPersistence
    ( Id
    , Entity
    , PersistenceException (..)
    , getId
    , persist
    , put
    , retrieve
    , retrieveAll
    , delete
    ) where
import           Data.Aeson       (FromJSON, ToJSON, 
                                   eitherDecodeFileStrict, 
                                   encodeFile, toJSON, decodeFileStrict)
import           Data.List hiding (delete)
import           Data.Typeable (Typeable, TypeRep, typeRep, Proxy)
import           System.Directory (listDirectory, removeFile)
import           Control.Exception 
import           System.Directory (doesFileExist)

-- | Identifier for an Entity
type Id = String

-- | exeption that may occur during persistence operations
data PersistenceException = 
    EntityNotFound
  | EntityAlreadyExists
  | InternalError
  deriving (Show)

instance Exception PersistenceException

-- | The Entity type class provides generic persistence to JSON files
class (ToJSON a, FromJSON a, Typeable a) => Entity a where

    -- | return the unique Id of the entity. This function must be implemented by type class instances.
    getId :: a -> Id

    -- | persist an entity of type a and identified by an Id to a json file
    persist :: a -> IO ()
    persist entity = do
        -- compute file path based on runtime type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) (getId entity)
        -- serialize entity as JSON and write to file
        encodeFile jsonFileName entity

    -- | persist an entity of type a and identified by an Id to a json file
    put :: Id -> a -> IO ()
    put id entity = do
        -- compute file path based on runtime type and given id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        -- serialize entity as JSON and write to file
        encodeFile jsonFileName entity

    -- | delete an entity of type a and identified by an Id to a json file
    delete :: Proxy a -> Id -> IO ()
    delete proxy id = do
        -- compute file path based on runtime type and entity id
        let jsonFileName = getPath (typeRep proxy) id
        -- remove file
        removeFile jsonFileName

    -- | load persistent entity of type a and identified by an Id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on entity type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        -- parse entity from JSON file
        decodeFile jsonFileName    
        
    -- | load all persistent entities of type a
    retrieveAll :: Maybe Int -> IO [a]
    retrieveAll maxRecords = do
        let tr = typeRep ([] :: [a])
        allFiles <- listDirectory dataDir
        let filteredFiles = filter (\fname -> isPrefixOf (show tr) fname && isSuffixOf ".json" fname) allFiles
        let files = case maxRecords of
                      Nothing -> filteredFiles
                      Just n  -> take n filteredFiles
        mapM (\fname -> decodeFile (dataDir ++ fname)) files

decodeFile :: FromJSON a => String -> IO a
decodeFile jsonFileName= do
  fileExists <- doesFileExist jsonFileName
  if fileExists
    then do
      eitherEntity <- eitherDecodeFileStrict jsonFileName
      case eitherEntity of
              Left msg -> throw InternalError
              Right e  -> return e
    else throw EntityNotFound

    
    

-- | compute path of data file
getPath :: TypeRep -> String -> String
getPath tr id = dataDir ++ show tr ++ "." ++ id ++ ".json"

dataDir :: FilePath
dataDir = ".stack-work/"
