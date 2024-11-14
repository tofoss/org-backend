{-# LANGUAGE OverloadedStrings #-}

module Auth (auth) where

import Servant.Auth.Server
import Models.User
import Servant

auth :: AuthResult User -> (User -> Handler a) -> Handler a
auth res func = case res of
  Authenticated user -> func user
  _ -> throwError err403 {errBody = "Invalid credentials"}

