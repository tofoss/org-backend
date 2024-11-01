module Crypto (generateHS512Key, hashPassword', verifyPassword) where

import Crypto.JOSE (getRandomBytes)
import Crypto.JOSE.JWK (fromOctets)
import Data.ByteString
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS

generateHS512Key :: IO JWTSettings
generateHS512Key = do
    secretKey <- getRandomBytes 64 :: IO ByteString  
    let jwk = fromOctets secretKey
    return $ defaultJWTSettings jwk


hashPassword' :: String -> IO (Maybe String)
hashPassword' password = do
  maybeHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack password)
  return $ fmap BS.unpack maybeHash

verifyPassword :: String -> String -> Bool
verifyPassword password hash = validatePassword (BS.pack hash) (BS.pack password) 

