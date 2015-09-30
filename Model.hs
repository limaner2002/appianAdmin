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