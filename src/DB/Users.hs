{-# LANGUAGE OverloadedStrings #-}

module DB.Users where

import Database.PostgreSQL.Simple 
import Models.User
import Data.Maybe (listToMaybe)

insertUser :: Connection -> String -> String -> IO Bool
insertUser conn _username _password = do
    result <- execute conn "INSERT INTO users (username, password) VALUES (?, ?)" (_username, _password)
    return (result > 0)


checkUserExists :: Connection -> String -> IO Bool
checkUserExists conn _username = do
    result <- query conn "SELECT 1 FROM users WHERE username = ?" (Only _username) :: IO [Only Int]
    return $ not (null result)

fetchHashedPassword :: Connection -> String -> IO (Maybe String)
fetchHashedPassword conn _username = do
    result <- query conn "SELECT password FROM users WHERE username = ?" (Only _username) :: IO [Only String]
    return $ listToMaybe $ map fromOnly result

fetchUser :: Connection -> String -> IO (Maybe User)
fetchUser conn _username = do
    results <- query conn "SELECT username FROM users WHERE username = ?" (Only _username)
    return $ listToMaybe results
