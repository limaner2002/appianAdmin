module TailFile where

{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals
{-# LANGUAGE FlexibleContexts #-}
import System.FSNotify
import Control.Concurrent (threadDelay)
import Data.Conduit
import System.IO (withBinaryFile, hFileSize, hSeek, IOMode(..), SeekMode(..))
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Data.Map as M
import Import hiding (atomically, withManager, (<>))
import Data.Monoid ((<>))
import System.FilePath (takeDirectory)
import Control.Monad.Logger
import Control.Monad.State
import System.Log.FastLogger (fromLogStr)
import qualified Data.ByteString.Char8 as C8

type TailFileM = StateT TailConf IO
data TailConf = TailConf
              { tailTLogUsers :: TWatchDirMap
              , tailTLogFiles :: TLogFileMap
              , tailCurrentPath :: FilePath
              , tailTChan :: TChan ChannelMessage
              }

delay :: Int
delay = truncate 30e6

pollInterval :: Int
pollInterval = truncate 5e6

myAction :: (MonadIO m) => TLogFileMap -> TailConf -> Event -> m ()
myAction tLogFiles _ (Added path time) = do
  putStrLn $ "File " <> pack path <> " was added"
  logFiles <- liftIO $ atomically $ readTVar tLogFiles
  case contains path logFiles of
    True -> setPos tLogFiles path 0
    False -> return ()
myAction tLogFiles tailConf (Modified path time) = do
  putStrLn $ "File " <> pack path <> " was modified"
  readDesiredFile tLogFiles path tailConf
myAction _ _ (Removed path time) = do
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

tailFile :: MonadIO m => TWatchDirMap -> TLogFileMap -> FilePath -> TailConf -> m ()
tailFile tLogUsers tLogFiles path tailConf = do
  let conf = defaultConfig { confUsePolling = True
                           , confPollInterval = pollInterval
                           }
  liftIO $ withManagerConf conf $ \mgr -> do
    readDesiredFile tLogFiles path tailConf
    -- start a watching job (in the background)
    watchDir
      mgr                    -- manager
      (takeDirectory path)   -- directory to watch
      (const True)           -- predicate
      (myAction tLogFiles tailConf)   -- action
    sleep tLogUsers tLogFiles path tailConf

sleep :: MonadIO m => TWatchDirMap -> TLogFileMap -> FilePath -> TailConf -> m ()
sleep tWatchDirMap tLogFileMap path tailConf = do
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
      liftIO $ runTail (pingSource $$ writeChannel) tailConf
      sleep tWatchDirMap tLogFileMap path tailConf

contains :: FilePath -> LogFileMap -> Bool
contains path posMap =
  case M.lookup path posMap of
    Nothing -> False
    Just _ -> True

readFilePart :: MonadIO m => Conduit Text m ChannelMessage
readFilePart = do
  mContents <- await
  case mContents of
    Nothing -> return ()
    Just contents -> do
                   yield $ Data contents
                   readFilePart

pingSource :: MonadIO m => Producer m ChannelMessage
pingSource =
    yield Ping

writeChannel :: Consumer ChannelMessage TailFileM ()
writeChannel = do
  mContents <- await
  case mContents of
    Nothing -> return ()
    Just contents -> do
                   channel <- gets tailTChan
                   liftIO $ atomically $ writeTChan channel contents
                   --liftIO $ print contents
                   writeChannel

getChannel :: MonadIO m => FilePath -> TLogFileMap -> m (TChan ChannelMessage)
getChannel path tLogFileMap = do
  logFileMap <- liftIO $ atomically $ readTVar tLogFileMap
  case lookup path logFileMap of
    Nothing -> error $ "No channel for file " `mappend` path
    Just (AppianLogMessage chan _) -> return chan

resetChannel :: MonadIO m => FilePath -> TLogFileMap -> m ()
resetChannel path tLogFileMap = do
  oldChan <- getChannel path tLogFileMap
  liftIO $ atomically $ do
         dChan <- dupTChan oldChan
         modifyTVar tLogFileMap $ \logFileMap ->
           M.insert path (AppianLogMessage dChan 0) logFileMap

readDesiredFile :: (MonadIO m) => TLogFileMap -> FilePath -> TailConf -> m ()
readDesiredFile tLogFiles path tailConf = do
  putStrLn $ "Reading file " <> pack path
  logFiles <- liftIO $ atomically $ readTVar tLogFiles
  putStrLn $ "posMap: " <> pack (show logFiles)
  res <- atomicLookup path tLogFiles
  putStrLn $ "Result: " <> pack (show res)
  case res of
    Just (AppianLogMessage channel _) -> do
      liftIO $ withBinaryFile path ReadMode $ \handle -> do
          previousPos <- liftIO $ getPos tLogFiles path
          size <- liftIO $ hFileSize handle
          if size < previousPos
            then do
              liftIO $ hSeek handle AbsoluteSeek 0
              resetChannel path tLogFiles
            else liftIO $ hSeek handle AbsoluteSeek previousPos

          liftIO $ setPos tLogFiles path size
--          liftIO $ putStrLn $ "
          runTail (sourceHandle handle $$ readFilePart
                       $= writeChannel) tailConf
    Nothing -> return ()

atomicLookup :: (MonadIO m, Ord k) => k -> TVar (M.Map k a) -> m (Maybe a)
atomicLookup key tMap = do
    mp <- liftIO $ atomically $ readTVar tMap
    return $ M.lookup key mp

runTail = evalStateT

