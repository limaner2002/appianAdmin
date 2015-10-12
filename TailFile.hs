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
import Yesod.WebSockets (sendTextData, sendPingE, webSockets, WebSocketsT)
import Import hiding (atomically, withManager, (<>))
import Data.Monoid ((<>))
import Foundation
import System.FilePath (takeDirectory)

delay = truncate 30e6

myAction :: (MonadIO m) => TLogFileMap -> Event -> m ()
myAction tLogFiles (Added path time) = do
  putStrLn $ "File " <> pack path <> " was added"
  logFiles <- liftIO $ atomically $ readTVar tLogFiles
  case contains path logFiles of
    True -> setPos tLogFiles path 0
    False -> return ()
myAction tLogFiles (Modified path time) = do
  putStrLn $ "File " <> pack path <> " was modified"
  readDesiredFile tLogFiles path
myAction _ (Removed path time) = do
  putStrLn $ "File " <> pack path <> " was removed"
  return ()

setPos :: MonadIO m => TLogFileMap -> FilePath -> Integer -> m ()
setPos tLogFiles path newPos =
    liftIO $ atomically $ modifyTVar tLogFiles $ \logFiles ->
        M.alter (\mVal ->
                   case mVal of
                     Nothing -> Nothing
                     Just (AppianLogMessage channel _) -> Just $ AppianLogMessage channel newPos
              ) path logFiles

getPos :: MonadIO m => TLogFileMap -> FilePath -> m Integer
getPos tLogFiles filePath = do
    posMap <- liftIO $ atomically $ readTVar tLogFiles
    case M.lookup filePath posMap of
      Nothing -> return 0
      Just (AppianLogMessage _ p) -> return p

tailFile :: MonadIO m => TWatchDirMap -> TLogFileMap -> FilePath -> m ()
tailFile tLogUsers tLogFiles path = do
  liftIO $ withManager $ \mgr -> do
    readDesiredFile tLogFiles path
    -- start a watching job (in the background)
    watchDir
      mgr                    -- manager
      (takeDirectory path)   -- directory to watch
      (const True)           -- predicate
      (myAction tLogFiles)   -- action
    sleep tLogUsers tLogFiles path

sleep :: MonadIO m => TWatchDirMap -> TLogFileMap -> FilePath -> m ()
sleep tWatchDirMap tLogFileMap path = do
  let pathDir = takeDirectory path
  liftIO $ threadDelay delay
  watchDirMap <- liftIO $ atomically $ readTVar tWatchDirMap
  liftIO $ putStrLn $ "watchDirMap: " <> pack (show watchDirMap)
  case lookup pathDir watchDirMap of
    Nothing -> do
        putStrLn $ "No longer watching " <> pack pathDir
        return ()
    Just _ -> do
      channel <- getChannel path tLogFileMap
      putStrLn $ "Pinging channel for directory " <> pack pathDir
      writeChannel channel $ Ping
      sleep tWatchDirMap tLogFileMap path

contains :: FilePath -> LogFileMap -> Bool
contains path posMap =
  case M.lookup path posMap of
    Nothing -> False
    Just _ -> True

readFilePart :: MonadIO m => TLogFileMap -> FilePath -> m Text
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
                           return contents

writeChannel :: (MonadIO m, Show a) => TChan a -> a -> m ()
writeChannel channel contents = do
  putStrLn $ "Writing " <> pack (show contents) <> " to channel"
  liftIO $ atomically $ writeTChan channel contents

getChannel :: MonadIO m => FilePath -> TLogFileMap -> m (TChan ChannelMessage)
getChannel path tLogFileMap = do
  logFileMap <- liftIO $ atomically $ readTVar tLogFileMap
  case lookup path logFileMap of
    Nothing -> error $ "No channel for file " `mappend` path
    Just (AppianLogMessage chan _) -> return chan

readDesiredFile :: (MonadIO m) => TLogFileMap -> FilePath -> m ()
readDesiredFile tLogFiles path = do
  putStrLn $ "Reading file " <> pack path
  logFiles <- liftIO $ atomically $ readTVar tLogFiles
  putStrLn $ "posMap: " <> pack (show logFiles)
  res <- atomicLookup path tLogFiles
  putStrLn $ "Result: " <> pack (show res)
  case res of
    Just (AppianLogMessage channel _) -> do
      contents <- readFilePart tLogFiles path
      writeChannel channel $ Data contents
    Nothing -> return ()

atomicLookup :: (MonadIO m, Ord k) => k -> TVar (M.Map k a) -> m (Maybe a)
atomicLookup key tMap = do
    mp <- liftIO $ atomically $ readTVar tMap
    return $ M.lookup key mp