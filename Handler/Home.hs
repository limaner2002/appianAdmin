module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "homepage")