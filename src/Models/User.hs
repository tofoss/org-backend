{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.User where
import Servant.Auth.JWT (ToJWT, FromJWT)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)
import Data.UUID

data User = User 
    { userId :: UUID 
    , username :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT, FromRow)

