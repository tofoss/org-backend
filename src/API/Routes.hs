{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module API.Routes where

import API.Requests.LoginRequest (LoginRequest)
import API.Requests.RegisterRequest
import API.Responses.RegisterResponse
import DB.Connection
import Database.PostgreSQL.Simple hiding ((:.))
import Handlers.Users
import Models.User
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import qualified Servant.Auth as SA
import Servant.Auth.Server
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

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
    registerUser conn
    :<|> loginHandler conn cookieSettings jwtSettings
    :<|> fooHandler

start :: IO ()
start = do
  conn <- initDB
  let port = 8080 :: Int

  jwtSecretKey <- generateKey
  let jwtSettings = defaultJWTSettings jwtSecretKey
  let cookieSettings = defaultCookieSettings
  let ctx = cookieSettings :. jwtSettings :. EmptyContext

  print $ "Server running on port " ++ show port
  homeDir <- getHomeDirectory
  let certPath = homeDir </> ".secrets/tls/cert.pem"
      keyPath = homeDir </> ".secrets/tls/secret-key.pem"
  runTLS (tlsSettings certPath keyPath) warpOpts (serveWithContext api ctx (server conn cookieSettings jwtSettings))
  where
    warpOpts = setPort 8080 defaultSettings
