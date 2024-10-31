{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Routes where

import Servant
import API.Requests.RegisterRequest
import API.Responses.RegisterResponse
import Handlers.Users (registerUser)
import DB.Connection
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

type API = "users" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse

api :: Proxy API
api = Proxy


start :: IO ()
start = do
    conn <- initDB 
    let port = 8080
    putStrLn $ "Server running on port " ++ show port
    homeDir <- getHomeDirectory
    let certPath = homeDir </> ".secrets/tls/cert.pem"
        keyPath  = homeDir </> ".secrets/tls/secret-key.pem"
    runTLS (tlsSettings certPath keyPath) warpOpts (serve api (registerUser conn))
    where 
          warpOpts = setPort 8080 defaultSettings
