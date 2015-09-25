module Handler.Home where

import qualified Data.Text as T
import Import hiding ((<>))
import Yesod.Form.Bootstrap3 ( renderBootstrap3
                             , withSmallInput
                             , BootstrapFormLayout (..)
                             )
import Yesod.Table (Table)
import qualified Yesod.Table as Table
import Data.Monoid
import qualified Data.Map as M

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  fileList <- getList
  let tableWidget = Table.buildBootstrap fileTable fileList
      files = zip (map fst fileList) ([1..] :: [Int])
  defaultLayout $ do
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  action <- lookupPostParam "action"
  checked <- lookupPostParams "checkbox"
  case (result, action) of
    (FormFailure _, Just "delete") -> do
                app <- getYesod
                deleteFile checked $ appFiles app
                redirect HomeR
    (FormSuccess fi, Just "upload") -> do
                app <- getYesod
                let filePath = "/tmp/" ++ (T.unpack $ fName)
                    files = appFiles app
                    fName = fileName fi
                fileMap <- liftIO $ readTVarIO files
                case M.lookup fName fileMap of
                  Nothing -> do
                          runResourceT $ fileSource fi $$ sinkFile filePath
                          addFile (appFiles app) (fileName fi) (T.pack filePath)
                  Just _ -> return ()
    _ -> return ()

  fileList <- getList
  let tableWidget = Table.buildBootstrap fileTable fileList
      files = zip (map fst fileList) ([1..] :: [Int])
  defaultLayout $ do
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "homepage")

sampleForm :: Form FileInfo
sampleForm = renderBootstrap3 BootstrapBasicForm $ fileAFormReq "Plugin File"

fileTable :: Table App (T.Text, T.Text)
fileTable = mempty
  <> Table.text "File Name" fst
  <> Table.text "File Path" snd
