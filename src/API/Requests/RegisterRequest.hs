{-# LANGUAGE DeriveGeneric #-}

module API.Requests.RegisterRequest where
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data RegisterRequest = RegisterRequest
    { username :: String
    , password :: String
    } deriving (Generic)

instance ToJSON RegisterRequest
instance FromJSON RegisterRequest

