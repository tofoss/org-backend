{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes (api, server) where

import API.Requests.LoginRequest (LoginRequest)
import API.Requests.RegisterRequest
import API.Responses.RegisterResponse
import Database.PostgreSQL.Simple hiding ((:.))
import Handlers.Users
import Models.User
import Servant
import qualified Servant.Auth as SA
import Servant.Auth.Server
import API.Requests.ArticleRequest (ArticleRequest)
import Models.Article (Article)
import Handlers.Articles (articleHandler)
import API.Responses.AuthStatusResponse

type API auths =
  "users" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse
    :<|> "users" :> "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Auth auths User :> "users" :> "auth" :> "status" :> Get '[JSON] AuthStatusResponse 
    :<|> Auth auths User :> "articles" :> ReqBody '[JSON] ArticleRequest :> Post '[JSON] Article

api :: Proxy (API '[SA.JWT, SA.Cookie])
api = Proxy

server :: Connection -> CookieSettings -> JWTSettings -> Server (API auths)
server conn cookieSettings jwtSettings =
    registerHandler conn
    :<|> loginHandler conn cookieSettings jwtSettings
    :<|> authStatusHandler
    :<|> articleHandler conn

