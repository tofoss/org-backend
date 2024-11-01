{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Users (registerHandler, loginHandler) where

import API.Requests.RegisterRequest
import API.Responses.RegisterResponse (RegisterResponse (..))
import Control.Monad.IO.Class
import DB.Users
import Database.PostgreSQL.Simple
import Models.User
import Servant
import Servant.Auth.Server
import API.Requests.LoginRequest (LoginRequest (..))
import Crypto (hashPassword', verifyPassword)

registerHandler :: Connection -> RegisterRequest -> Handler RegisterResponse
registerHandler conn RegisterRequest {..} = do
    userExists <- liftIO $ checkUserExists conn username
    if userExists
        then throwError err409 { errBody = "Username already exists" }
        else handleUserRegistration conn username password

handleUserRegistration :: Connection -> String -> String -> Handler RegisterResponse
handleUserRegistration conn username password = do
    hashedPassword <- hashPassword password
    registerUser conn username hashedPassword

hashPassword :: String -> Handler String
hashPassword password = do
    maybeHashedPassword <- liftIO $ hashPassword' password
    case maybeHashedPassword of
        Nothing   -> throwError err500 { errBody = "Password hashing failed" }
        Just hash -> return hash

registerUser :: Connection -> String -> String -> Handler RegisterResponse
registerUser conn username hashedPassword = do
    success <- liftIO $ insertUser conn username hashedPassword
    if success
        then return RegisterResponse { message = "Success" }
        else throwError err500 { errBody = "User registration failed" }

loginHandler ::
  Connection ->
  CookieSettings ->
  JWTSettings ->
  LoginRequest ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler conn cookieSettings jwtSettings loginRequest = do
  authResult <- liftIO $ authCheck conn loginRequest
  loginUser cookieSettings jwtSettings authResult

loginUser ::
  CookieSettings ->
  JWTSettings ->
  AuthResult User ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginUser cookieSettings jwtSettings authResult  =
  case authResult of
    Authenticated user -> createCookies cookieSettings jwtSettings user
    _ -> throwError err403 {errBody = "Invalid credentials"}

createCookies ::
  CookieSettings ->
  JWTSettings ->
  User ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
createCookies cookieSettings jwtSettings user = do
  cookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
  case cookies of
    Nothing -> throwError err500 {errBody = "Failed to create session"}
    Just c -> return $ c NoContent

authCheck :: Connection -> LoginRequest -> IO (AuthResult User)
authCheck conn LoginRequest {..} = do
  result <- verifyUser conn loginUsername loginPassword
  pure $ maybe Indefinite Authenticated result

verifyUser :: Connection -> String -> String -> IO (Maybe User)
verifyUser conn username password = do
  maybePassword <- liftIO $ fetchHashedPassword conn username
  case maybePassword of
    Nothing -> return Nothing
    Just hash -> do
      if not (verifyPassword password hash)
        then do
        return Nothing
        else do
          liftIO $ fetchUser conn username
