module User.Hash (
  Hash, hash, fromString, toString
) where
{-
  A simple module for hashes.
  Hashes can be created and compared.
  The fromString and toString methods are only imported for saving/loading hashes.
  This way some parts of the program will not be able to leak a hash.
-}
import qualified Codec.Digest.SHA as S
import qualified Data.ByteString.UTF8 as BS (fromString, ByteString)

newtype Hash = Hash String
instance Eq Hash where
  (Hash a) == (Hash b) = a == b

hash :: String -> Hash
hash = Hash . S.showBSasHex . S.hash S.SHA512 . BS.fromString

fromString :: String -> Hash
fromString = Hash

toString :: Hash -> String
toString (Hash bs) = bs
