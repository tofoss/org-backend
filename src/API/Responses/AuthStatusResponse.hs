{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Responses.AuthStatusResponse (AuthStatusResponse(..)) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data AuthStatusResponse = AuthStatusResponse
  { loggedIn :: Bool,
    username :: String
  }
  deriving (Generic, ToJSON)
