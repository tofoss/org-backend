{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API.Routes where

import Servant
import API.Requests.RegisterRequest
import API.Responses.RegisterResponse
import Handlers.Users (registerUser)
import DB.Connection
import Network.Wai.Handler.Warp (run)

type API = "users" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] RegisterResponse

api :: Proxy API
api = Proxy


start :: IO ()
start = do
    conn <- initDB 
    let port = 8080
    putStrLn $ "Server running on port " ++ show port
    run port (serve api (registerUser conn))
