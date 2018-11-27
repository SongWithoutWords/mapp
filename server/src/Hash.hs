module Hash
  ( HashedPasswordAndSalt(..)
  , hashNew
  , hashEqual
  ) where

import qualified Crypto.KDF.Argon2 as A2
import Crypto.Error
import Crypto.Random(getRandomBytes)

import Import -- .NoFoundation

data HashedPasswordAndSalt = HashedPasswordAndSalt
  { hashedPassword :: ByteString
  , salt :: ByteString
  } deriving(Eq, Show)

hashLengthBytes :: Int
hashLengthBytes = 64 -- 64 bytes => 512 bits

-- Creates a new slat and uses it to hash a plaintext password
hashNew :: Text -> Handler HashedPasswordAndSalt
hashNew plainText = do
  salt <- liftIO $ getRandomBytes hashLengthBytes
  case A2.hash A2.defaultOptions (encodeUtf8 plainText) salt hashLengthBytes of
    CryptoPassed hash -> pure $ HashedPasswordAndSalt hash salt
    CryptoFailed e -> error $ show e

-- Hashes the plaintext password with the salt and compares it with the hashed password
hashEqual :: Text -> HashedPasswordAndSalt -> Bool
hashEqual plainText (HashedPasswordAndSalt hashedPassword salt) =
  case A2.hash A2.defaultOptions (encodeUtf8 plainText) salt hashLengthBytes of
    CryptoPassed hash -> hash == hashedPassword
    CryptoFailed e -> error $ show e
