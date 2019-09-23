{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Strict              #-}
module JsonPersistence
    ( Id
    , Entity
    , PersistenceException (..)
    , getId
    , post
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
import           System.Directory (listDirectory, removeFile, doesFileExist)
import           Control.Exception

-- | Identifier for an Entity
type Id = String

-- | exeptions that may occur during persistence operations
data PersistenceException = 
    EntityNotFound String
  | EntityAlreadyExists String
  | InternalError String
  deriving (Show)

instance Exception PersistenceException



-- | The Entity type class provides generic persistence to JSON files
class (ToJSON a, FromJSON a, Typeable a) => Entity a where

    -- | return the unique Id of the entity. This function must be implemented by type class instances.
    getId :: a -> Id

    -- | persist a new entity of type a and identified by an Id to a json file
    post :: a -> IO ()
    post entity = do
        -- compute file path based on runtime type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) (getId entity)
        fileExists <- doesFileExist jsonFileName
        if fileExists
          then throw $ EntityAlreadyExists ("entity record already exists: " ++ jsonFileName)
          else encodeFile jsonFileName entity     -- serialize entity as JSON and write to file

    -- | update an entity of type a and identified by an Id to a json file
    put :: Id -> a -> IO ()
    put id entity = do
        -- compute file path based on runtime type and given id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        fileExists <- doesFileExist jsonFileName
        if fileExists
          then encodeFile jsonFileName entity     -- serialize entity as JSON and write to file
          else throw $ EntityNotFound ("could not update as entity was not found: " ++ jsonFileName)


    -- | delete an entity of type a and identified by an Id to a json file
    delete :: Proxy a -> Id -> IO ()
    delete proxy id = do
        -- compute file path based on runtime type and entity id
        let jsonFileName = getPath (typeRep proxy) id
        fileExists <- doesFileExist jsonFileName
        if fileExists
          then removeFile jsonFileName
          else throw $ EntityNotFound ("could not delete as entity was not found: " ++ jsonFileName)

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
              Left msg -> throw (InternalError $ "could not parse data: " ++ msg)
              Right e  -> return e
    else throw (EntityNotFound $ "could not find: " ++ jsonFileName)

    
-- | compute path of data file
getPath :: TypeRep -> String -> String
getPath tr id = dataDir ++ show tr ++ "." ++ id ++ ".json"

dataDir :: FilePath
dataDir = ".stack-work/"
