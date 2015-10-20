module Handler.Plugins where

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
getPluginsR :: Handler Html
getPluginsR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  fileList <- getList
  let files = zip (map pluginFileName fileList) ([1..] :: [Int])
  defaultLayout $ do
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "plugins")

postPluginsR :: Handler Html
postPluginsR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  action <- lookupPostParam "action"
  checked <- lookupPostParams "checkbox"
  case (result, action) of
    (FormFailure _, Just "delete") -> do
                app <- getYesod
                deleteFile checked
                redirect PluginsR
    (FormSuccess fi, Just "upload") -> do
                app <- getYesod
                let filePath = T.concat ["/opt/appian/_admin/plugins/", fName]
                    fName = fileName fi
                runResourceT $ fileSource fi $$ sinkFile (T.unpack filePath)
                addFile (Plugin fName filePath)
    _ -> return ()

  fileList <- getList
  let files = zip (map pluginFileName fileList) ([1..] :: [Int])
  defaultLayout $ do
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "plugins")

sampleForm :: Form FileInfo
sampleForm = renderBootstrap3 BootstrapBasicForm $ fileAFormReq "Plugin File"

fileTable :: Table App (T.Text, T.Text)
fileTable = mempty
  <> Table.text "File Name" fst
  <> Table.text "File Path" snd
