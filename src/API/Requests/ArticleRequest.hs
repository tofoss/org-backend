{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.Requests.ArticleRequest (ArticleRequest(..)) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)
import Data.UUID

data ArticleRequest = ArticleRequest
    { articleId :: Maybe UUID 
    , articleContent :: String
    , articlePublished :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)
