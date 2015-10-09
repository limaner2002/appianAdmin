module TailFile where

{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals
{-# LANGUAGE FlexibleContexts #-}
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Conduit
--import Data.Conduit.Binary (sourceHandle)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import System.IO (withBinaryFile, hFileSize, hSeek, IOMode(..), SeekMode(..))
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.Map as M
import Yesod.WebSockets (sendTextData, webSockets, WebSocketsT)
import Import hiding (atomically, withManager, (<>))
import Data.Monoid ((<>))

type PosMap = M.Map FilePath Integer
type FilePosition = TVar PosMap

initialFilePosition :: IO FilePosition
initialFilePosition = atomically $ newTVar mempty

myAction :: (MonadIO m) => TChan AppianLogMessage -> FilePosition -> Event -> m ()
myAction _ pos (Added path time) = do
  putStrLn $ "File " <> pack path <> " was added"
  posMap <- liftIO $ atomically $ readTVar pos
  case contains path posMap of
    True -> setPos pos path 0
    False -> return ()
myAction channel pos (Modified path time) = do
  putStrLn $ "File " <> pack path <> " was modified"
  readDesiredFile channel pos path
myAction _ pos (Removed path time) = do
  putStrLn $ "File " <> pack path <> " was removed"
  return ()

setPos :: MonadIO m => FilePosition -> FilePath -> Integer -> m ()
setPos pos filePath newPos =
    liftIO $ atomically $ modifyTVar pos $ \posMap ->
        M.insert filePath newPos posMap

getPos :: MonadIO m => FilePosition -> FilePath -> m Integer
getPos tPos filePath = do
    posMap <- liftIO $ atomically $ readTVar tPos
    case M.lookup filePath posMap of
      Nothing -> return 0
      Just p -> return p

tailFile :: MonadIO m => TChan AppianLogMessage -> TVar Int -> FilePath -> m ()
tailFile channel tLogUsers path = do
  filePos <- liftIO $ initialFilePosition
  setPos filePos path 0
  liftIO $ withManager $ \mgr -> do
    readDesiredFile channel filePos path
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "/tmp/"          -- directory to watch
      (const True) -- predicate
      (myAction channel filePos)     -- action
    sleep tLogUsers

sleep :: MonadIO m => TVar Int -> m ()
sleep tLogUsers = do
    liftIO $ threadDelay 1000000
    users <- liftIO $ atomically $ readTVar tLogUsers
    case users of
      0 -> return ()
      n -> sleep tLogUsers

contains :: FilePath -> PosMap -> Bool
contains path posMap =
  case M.lookup path posMap of
    Nothing -> False
    Just _ -> True

readFilePart :: MonadIO m => FilePosition -> FilePath -> m AppianLogMessage
readFilePart pos path =
    liftIO $ withBinaryFile path ReadMode $ \handle ->
        sourceHandle handle $$ do
          liftIO $ putStrLn "Reading file now"
          previousPos <- liftIO $ getPos pos path
          size <- liftIO $ hFileSize handle
          liftIO $ hSeek handle AbsoluteSeek previousPos
          mContents <- await
          liftIO $ setPos pos path size
          print mContents
          case mContents of
            Nothing -> return mempty
            Just contents -> do
                           return $ AppianLogMessage contents size

writeChannel :: (MonadIO m, Show a) => TChan a -> a -> m ()
writeChannel channel contents = do
  putStrLn $ "Writing " <> pack (show contents) <> " to channel"
  liftIO $ atomically $ writeTChan channel contents

readDesiredFile :: (MonadIO m) => TChan AppianLogMessage -> FilePosition -> FilePath -> m ()
readDesiredFile channel pos path = do
  putStrLn $ "Reading file " <> pack path
  posMap <- liftIO $ atomically $ readTVar pos
  putStrLn $ "posMap: " <> pack (show posMap)
  case contains path posMap of
    True -> do
      contents <- readFilePart pos path
      writeChannel channel contents
    False -> return ()