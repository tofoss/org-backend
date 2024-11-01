{-# LANGUAGE DataKinds #-}

module App where

import API.Routes
import Crypto (generateHS512Key)
import DB.Connection
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.Auth.Server
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

app :: IO ()
app = do
  conn <- initDB
  let port = 8080 :: Int
  jwtSettings <- generateHS512Key
  let cookieSettings = defaultCookieSettings
  let ctx = cookieSettings :. jwtSettings :. EmptyContext

  homeDir <- getHomeDirectory
  let certPath = homeDir </> ".secrets/tls/cert.pem"
      keyPath = homeDir </> ".secrets/tls/secret-key.pem"

  let tls = tlsSettings certPath keyPath
  let warpSettings = setPort 8080 defaultSettings
  let application = serveWithContext api ctx (server conn cookieSettings jwtSettings)

  print $ "Server running on port " ++ show port

  runTLS tls warpSettings application
