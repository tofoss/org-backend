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
import Servant

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

hashPassword' :: String -> IO (Maybe String)
hashPassword' password = do
  maybeHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack password)
  return $ fmap BS.unpack maybeHash

verifyPassword :: String -> String -> Bool
verifyPassword password hash = validatePassword (BS.pack password) (BS.pack hash)
