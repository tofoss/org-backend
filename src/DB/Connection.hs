module DB.Connection where

import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)


initDB :: IO Connection
initDB = do
    user     <- fromMaybe "postgres" <$> lookupEnv "POSTGRES_USER"
    password <- fromMaybe "password" <$> lookupEnv "POSTGRES_PASSWORD"
    database <- fromMaybe "postgres" <$> lookupEnv "POSTGRES_DB"
    host     <- fromMaybe "127.0.0.1" <$> lookupEnv "POSTGRES_HOST"
    port     <- maybe 5432 read <$> lookupEnv "POSTGRES_PORT"

    connect defaultConnectInfo
      { connectDatabase = database
      , connectUser     = user
      , connectPassword = password
      , connectHost     = host
      , connectPort     = port
      }

