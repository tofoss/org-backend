{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

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

type API auths =
  "users" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse
    :<|> "users" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Auth auths User :> "foo" :> Get '[JSON] String

api :: Proxy (API '[SA.JWT, SA.Cookie])
api = Proxy

fooHandler :: AuthResult User -> Handler String
fooHandler (Authenticated User{..}) = return $ "Hello " <> username <> "!"
fooHandler _ = return "Unauthorized"

server :: Connection -> CookieSettings -> JWTSettings -> Server (API auths)
server conn cookieSettings jwtSettings =
    registerHandler conn
    :<|> loginHandler conn cookieSettings jwtSettings
    :<|> fooHandler

