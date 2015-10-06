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
import Import hiding (atomically, withManager)

type PosMap = M.Map FilePath Integer
type FilePosition = TVar PosMap

initialFilePosition :: IO FilePosition
initialFilePosition = atomically $ newTVar mempty

myAction :: (MonadIO m) => TChan ByteString -> FilePosition -> Event -> m ()
myAction _ pos (Added path time) = do
  posMap <- liftIO $ atomically $ readTVar pos
  case contains path posMap of
    True -> setPos pos path 0
    False -> return ()
myAction channel pos (Modified path time) = do
  posMap <- liftIO $ atomically $ readTVar pos
  case contains path posMap of
    True ->
        liftIO $ withBinaryFile path ReadMode $ \handle ->
            sourceHandle handle $$ do
               previousPos <- liftIO $ getPos pos path
               size <- liftIO $ hFileSize handle
               liftIO $ hSeek handle AbsoluteSeek previousPos
               mContents <- await
               case mContents of
                 Nothing -> return ()
                 Just contents -> do
                                liftIO $ atomically $ writeTChan channel contents
               liftIO $ setPos pos path size
    False -> return ()

myAction _ pos (Removed path time) = return ()

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

tailFile :: MonadIO m => TChan ByteString -> TVar Int -> FilePath -> m ()
tailFile channel tLogUsers path = do
  filePos <- liftIO $ initialFilePosition
  setPos filePos path 0
  liftIO $ withManager $ \mgr -> do
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

readFile :: MonadIO m => Integer -> FilePath -> m Text
readFile pos path =
    liftIO $ withBinaryFile path ReadMode $ \handle ->
        sourceHandle handle $$ do
          previousPos <- liftIO $ getPos pos path
          size <- liftIO $ hFileSize handle
          liftIO $ hSeek handle AbsoluteSeek previousPos
          mContents <- await
          case mContents of
            Nothing -> return ()
            Just contents -> do
                           liftIO $ atomically $ writeTChan channel contents
            liftIO $ setPos pos path size
