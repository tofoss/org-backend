{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.Article (Article(..)) where
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)
import Data.UUID
import Data.Time (UTCTime)

data Article = Article 
    { articleId :: UUID 
    , articleUserId :: UUID
    , articleTitle :: String
    , articleContent :: String
    , articleCreatedAt :: UTCTime
    , articleUpdatedAt :: UTCTime
    , articlePublishedAt :: Maybe UTCTime
    , articlePublished :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)
