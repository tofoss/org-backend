{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Articles (articleHandler, fetchUsersArticlesHandler) where

import qualified API.Requests.ArticleRequest as Req
import Auth (auth)
import Control.Monad.IO.Class
import DB.Articles (fetchArticle, upsertArticle, fetchUsersArticles)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
import Models.Article
import Models.User (User (..))
import Servant
import Servant.Auth.Server (AuthResult (..))
import Prelude hiding (id)


fetchUsersArticlesHandler :: Connection -> AuthResult User -> Handler [Article]
fetchUsersArticlesHandler conn authResult = auth authResult $ \user -> liftIO $ fetchUsersArticles conn (userId user)

articleHandler :: Connection -> AuthResult User -> Req.ArticleRequest -> Handler Article
articleHandler conn authResult req = auth authResult $ handleArticleReq conn req

handleArticleReq :: Connection -> Req.ArticleRequest -> User -> Handler Article
handleArticleReq conn req user = case Req.articleId req of
  Just id -> updateArticle conn req user id
  Nothing -> createArticle conn req user

createArticle :: Connection -> Req.ArticleRequest -> User -> Handler Article
createArticle conn req user = do
  currentTime <- liftIO getCurrentTime
  id <- liftIO nextRandom

  let article =
        Article
          { articleId = id,
            articlePublished = Req.articlePublished req,
            articlePublishedAt = if Req.articlePublished req then Just currentTime else Nothing,
            articleUpdatedAt = currentTime,
            articleCreatedAt = currentTime,
            articleContent = Req.articleContent req,
            articleTitle = "title",
            articleUserId = userId user
          }

  maybeArticle <- liftIO $ upsertArticle conn article
  unwrap maybeArticle

updateArticle :: Connection -> Req.ArticleRequest -> User -> UUID -> Handler Article
updateArticle conn req user id = do
  article <- liftIO $ fetchArticle conn id (userId user)
  updateArticle' conn req article

updateArticle' :: Connection -> Req.ArticleRequest -> Maybe Article -> Handler Article
updateArticle' _ _ Nothing = throwError err403 {errBody = "Could not update article - Invalid credentials"}
updateArticle' conn req (Just article) = do
  currentTime <- liftIO getCurrentTime

  let updatedArticle =
        article
          { articlePublished = Req.articlePublished req,
            articlePublishedAt = publishedAt article req currentTime,
            articleUpdatedAt = currentTime,
            articleContent = Req.articleContent req,
            articleTitle = "title"
          }

  maybeArticle <- liftIO $ upsertArticle conn updatedArticle
  unwrap maybeArticle

publishedAt :: Article -> Req.ArticleRequest -> UTCTime -> Maybe UTCTime
publishedAt article req now
  | articlePublished article == Req.articlePublished req = articlePublishedAt article
  | Req.articlePublished req = Just now
  | otherwise = articlePublishedAt article

unwrap :: Maybe a -> Handler a
unwrap (Just x) = return x
unwrap Nothing = throwError err500 {errBody = "Internal server error - failed to fetch article"}
