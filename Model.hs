module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data AppianLog = JBoss | Application
               deriving Read

data ChannelMessage = Ping | Data Text

data AppianLogMessage = AppianLogMessage
                      { channel :: TChan ChannelMessage
                      , position :: Integer
                      }

instance Show AppianLogMessage where
    show (AppianLogMessage _ position) = show position

instance Show ChannelMessage where
    show Ping = "Ping"
    show (Data txt) = "Data:\n" <> unpack txt

instance Show AppianLog where
    show JBoss = "JBoss"
    show Application = "Application"

instance Eq AppianLog where
    (==) JBoss JBoss = True
    (==) Application Application = True
    (==) _ _ = False

    (/=) JBoss JBoss = False
    (/=) Application Application = False
    (/=) _ _ = True

instance PathPiece AppianLog where
    fromPathPiece val
        | val == "JBoss" = Just JBoss
        | val == "Application" = Just Application
        | otherwise = Nothing

    toPathPiece log = pack $ show log

instance ToJSON ChannelMessage where
    toJSON Ping = object ["type" .= ("Ping" :: Text)]
    toJSON (Data contents) =
        object ["type" .= ("Data" :: Text), "contents" .= contents]

-- instance ToJSON AppianLogMessage where
--     toJSON (AppianLogMessage contents position) =
--         object ["contents" .= contents, "position" .= position]

-- instance Monoid AppianLogMessage where
--     mempty = AppianLogMessage mempty 0

