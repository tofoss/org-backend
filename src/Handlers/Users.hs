{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Users where

import API.Requests.RegisterRequest
import API.Responses.RegisterResponse (RegisterResponse (..))
import Control.Monad.IO.Class
import Crypto.BCrypt
import DB.Users
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Models.User
import Servant
import Servant.Auth.Server
import API.Requests.LoginRequest (LoginRequest (..))

registerUser :: Connection -> RegisterRequest -> Handler RegisterResponse
registerUser conn RegisterRequest {..} = do
  userExists <- liftIO $ checkUserExists conn username
  if userExists
    then throwError err409 {errBody = "Username already exists"}
    else do
      maybeHashedPassword <- liftIO $ hashPassword' password
      case maybeHashedPassword of
        Nothing -> throwError err500
        Just hash -> do
          success <- liftIO $ insertUser conn username hash
          if success
            then return RegisterResponse {message = "Success"}
            else throwError err500

loginHandler ::
  Connection ->
  CookieSettings ->
  JWTSettings ->
  LoginRequest ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler conn cookieSettings jwtSettings loginRequest = do
  authResult <- liftIO $ authCheck conn loginRequest
  case authResult of
    Authenticated user -> do
      mCookie <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mCookie of
        Nothing -> throwError err500 {errBody = "Failed to create session"}
        Just cookie -> return $ cookie NoContent
    _ -> throwError err403 {errBody = "Invalid credentials"}

hashPassword' :: String -> IO (Maybe String)
hashPassword' password = do
  maybeHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack password)
  return $ fmap BS.unpack maybeHash

verifyPassword :: String -> String -> Bool
verifyPassword password hash = validatePassword (BS.pack hash) (BS.pack password) 

authCheck :: Connection -> LoginRequest -> IO (AuthResult User)
authCheck conn LoginRequest {..} = do
  result <- verifyUser conn loginUsername loginPassword
  pure $ maybe Indefinite Authenticated result

verifyUser :: Connection -> String -> String -> IO (Maybe User)
verifyUser conn _username _password = do
  maybePassword <- liftIO $ fetchHashedPassword conn _username
  case maybePassword of
    Nothing -> return Nothing
    Just hash -> do
      if not (verifyPassword _password hash)
        then do
        return Nothing
        else do
          liftIO $ fetchUser conn _username
