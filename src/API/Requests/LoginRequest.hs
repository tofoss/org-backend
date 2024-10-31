{-# LANGUAGE DeriveGeneric #-}

module API.Requests.LoginRequest where
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data LoginRequest = LoginRequest
    { loginUsername :: String
    , loginPassword :: String
    } deriving (Eq, Generic)

instance ToJSON LoginRequest
instance FromJSON LoginRequest

