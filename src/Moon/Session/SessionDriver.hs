module Moon.Session.SessionDriver (SessionDriver(..), MemorySessionDriver(..), FileSessionDriver(..), inMemoryStore) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Serialize (encode, decode)
import Data.ByteString (readFile, writeFile)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)

{-# NOINLINE inMemoryStore #-}
inMemoryStore :: Control.Concurrent.STM.TVar (Map Text (Map String String))
inMemoryStore = unsafePerformIO $ Control.Concurrent.STM.newTVarIO Map.empty

{- The SessionDriver class -}
class SessionDriver a where
    getSessionData :: a -> Text -> IO (Maybe (Map String String))
    updateSessionData :: a -> Text -> Map String String -> IO ()

{- Define the MemorySessionDriver datatype -}
data MemorySessionDriver = MemorySessionDriver {
    memoryStore :: Control.Concurrent.STM.TVar (Map Text (Map String String))
}

{- The MemorySessionDriver implementation -}
instance SessionDriver MemorySessionDriver where
    -- Retrieve session data
    getSessionData driver sid = Control.Concurrent.STM.atomically $ do
        store <- Control.Concurrent.STM.readTVar (memoryStore driver)
        return $ Map.lookup sid store
    -- Update session data
    updateSessionData driver sid newData = Control.Concurrent.STM.atomically $ do
        store <- Control.Concurrent.STM.readTVar (memoryStore driver)
        Control.Concurrent.STM.writeTVar (memoryStore driver) $ Map.insert sid newData store


{- Define the FileSessionDriver datatype -}
data FileSessionDriver = FileSessionDriver {
    storagePath :: FilePath -- The path to store session data
}

{- The FileSessionDriver implementation -}
instance SessionDriver FileSessionDriver where
    -- Retrieve session data ("driver" is the FileSessionDriver containing the session path)
    getSessionData driver sid = do
        -- Concatenate the session path with the session ID
        let filePath = storagePath driver </> T.unpack sid
        -- Check if the file exists
        exists <- System.Directory.doesFileExist filePath
        -- If the file exists, read the session data...
        if exists
            then do
                content <- Data.ByteString.readFile filePath
                case Data.Serialize.decode content of
                    Right dataMap -> return $ Just dataMap -- If the data is successfully decoded, return it
                    Left _ -> return Nothing -- If the data is not successfully decoded, return Nothing
        -- ...otherwise, return Nothing
        else return Nothing

    -- Update session data; @todo file locking must be implemented
    updateSessionData driver sid newData = do
        let filePath = storagePath driver </> T.unpack sid
        let tempFilePath = filePath ++ ".tmp"
        System.Directory.createDirectoryIfMissing True (storagePath driver)
        Data.ByteString.writeFile tempFilePath (Data.Serialize.encode newData)
        System.Directory.renameFile tempFilePath filePath