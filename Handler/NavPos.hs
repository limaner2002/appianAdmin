module Handler.NavPos where

import Import
import Text.Julius (rawJS)

getNavPosR :: Int -> Handler Html
getNavPosR navIdx = defaultLayout $ do
               aDomId <- newIdent
               setTitle "Appian Plugins"
               $(widgetFile "homepage")