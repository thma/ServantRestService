{-# LANGUAGE ScopedTypeVariables #-}
module JsonPersistence
    ( Id
    , Entity
    , getId
    , persist
    , retrieve
    , retrieveAll
    ) where
import           Data.Aeson       (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile, toJSON)
import           Data.List
import           Data.Typeable
import           System.Directory (listDirectory)

-- | Identifier for an Entity
type Id = String

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

    -- | load persistent entity of type a and identified by an Id
    retrieve :: Id -> IO a
    retrieve id = do
        -- compute file path based on entity type and entity id
        let jsonFileName = getPath (typeRep ([] :: [a])) id
        -- parse entity from JSON file
        decodeFile jsonFileName

    -- | load all persistent entities of type a
    retrieveAll :: IO [a]
    retrieveAll = do
        let tr = typeRep ([] :: [a])
        allFiles <- listDirectory dataDir
        let filteredFiles = filter (\fname -> isPrefixOf (show tr) fname && isSuffixOf ".json" fname) allFiles
        mapM (\fname -> decodeFile (dataDir ++ fname)) filteredFiles

decodeFile :: FromJSON a => String -> IO a
decodeFile jsonFileName= do
    eitherEntity <- eitherDecodeFileStrict jsonFileName
    case eitherEntity of
        Left msg -> fail msg
        Right e  -> return e

-- | compute path of data file
getPath :: TypeRep -> String -> String
getPath tr id = dataDir ++ show tr ++ "." ++ id ++ ".json"

dataDir :: FilePath
dataDir = ".stack-work/"
