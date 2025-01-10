{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import API.Routes
import Cli (Options (..), args)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto (generateHS512Key)
import DB.Connection
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Network.Wai.Middleware.RequestLogger

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:5173"], True)
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsExposedHeaders = Just ["Authorization"]
    , corsMaxAge = Just 3600
    }

app :: IO ()
app = do
  conn <- initDB
  let port = 8080 :: Int
  jwtSettings <- generateHS512Key
  let cookieSettings = defaultCookieSettings
  let ctx = cookieSettings :. jwtSettings :. EmptyContext
  options <- liftIO args

  let application = serveWithContext api ctx (server conn cookieSettings jwtSettings)

  if devMode options
    then do
      print $ "Server running in development mode on port " ++ show port
      run port (logStdoutDev (cors (const $ Just corsPolicy) application))
    else do
      homeDir <- getHomeDirectory
      let certPath = homeDir </> ".secrets/tls/cert.pem"
          keyPath = homeDir </> ".secrets/tls/secret-key.pem"

      let tls = tlsSettings certPath keyPath
      let warpSettings = setPort 8080 defaultSettings

      print $ "Server running on port " ++ show port

      runTLS tls warpSettings (logStdout (simpleCors application))
