{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.User where
import Servant.Auth.JWT (ToJWT, FromJWT)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)

data User = User 
    { 
     username :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToJWT, FromJWT, FromRow)

