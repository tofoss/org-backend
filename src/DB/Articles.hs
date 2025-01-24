{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Articles (upsertArticle, fetchArticle, fetchUsersArticles) where

import Database.PostgreSQL.Simple
import Models.Article (Article (..))
import Data.UUID

upsertArticle :: Connection -> Article -> IO (Maybe Article)
upsertArticle conn Article {..} = do
  let stmt =
        "INSERT INTO articles (id, user_id, title, content, created_at, updated_at, published_at, published) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
        \ON CONFLICT (id) DO UPDATE SET \
        \user_id = EXCLUDED.user_id, \
        \title = EXCLUDED.title, \
        \content = EXCLUDED.content, \
        \updated_at = EXCLUDED.updated_at, \
        \published_at = EXCLUDED.published_at, \
        \published = EXCLUDED.published \
        \RETURNING id, user_id, title, content, created_at, updated_at, published_at, published"
      params =
        ( articleId,
          articleUserId,
          articleTitle,
          articleContent,
          articleCreatedAt,
          articleUpdatedAt,
          articlePublishedAt,
          articlePublished
        )

  result <- query conn stmt params
  case result of
    [article] -> return (Just article)
    _ -> return Nothing

fetchArticle :: Connection -> UUID -> UUID -> IO (Maybe Article)
fetchArticle conn articleId userId = do
    let stmt = "select id, user_id, title, content, created_at, updated_at, published_at, published from articles where id = ? and user_id = ?"
        params = (articleId, userId)

    result <- query conn stmt params
    case result of
        [article] -> return (Just article)
        _ -> return Nothing


fetchUsersArticles :: Connection -> UUID -> IO [Article]
fetchUsersArticles conn userId = do
    let stmt = "select id, user_id, title, content, created_at, updated_at, published_at, published from articles where user_id = ?"

    query conn stmt (Only userId)
