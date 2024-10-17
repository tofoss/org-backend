{-# LANGUAGE OverloadedStrings #-}

module DB.Users where

import Database.PostgreSQL.Simple 

insertUser :: Connection -> String -> String -> IO Bool
insertUser conn username password = do
    result <- execute conn "INSERT INTO users (username, password) VALUES (?, ?)" (username, password)
    return (result > 0)


checkUserExists :: Connection -> String -> IO Bool
checkUserExists conn username = do
    result <- query conn "SELECT 1 FROM users WHERE username = ?" (Only username) :: IO [Only Int]
    return $ not (null result)
