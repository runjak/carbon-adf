{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.Hash (
  Hash, hash, toHash, fromHash, hash'
) where
{-
  A simple module for hashes.
  Hashes can be created and compared.
  The fromString and toString methods are only imported for saving/loading hashes.
  This way some parts of the program will not be able to leak a hash.
-}
import qualified Codec.Digest.SHA as S
import qualified Data.ByteString.UTF8 as BS (fromString, ByteString)

import OpenBrain.Data.Salt

newtype Hash = Hash String deriving (Eq, Ord, Show)

{-
  Uses hash' but adds a Salt.
  It's better to use Salts when possible.
-}
hash :: Salt -> String -> Hash
hash salt str = hash' . (++str) $ fromSalt salt

{-
  hash without a Salt
-}
hash' :: String -> Hash
hash' = Hash . S.showBSasHex . S.hash S.SHA512 . BS.fromString

toHash :: String -> Hash
toHash = Hash

fromHash :: Hash -> String
fromHash (Hash s) = s
