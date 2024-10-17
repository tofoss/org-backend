{-# LANGUAGE DeriveGeneric #-}

module API.Responses.RegisterResponse where

import GHC.Generics
import Data.Aeson (ToJSON)

data RegisterResponse = RegisterResponse
  { message :: String
  }
  deriving (Generic)

instance ToJSON RegisterResponse