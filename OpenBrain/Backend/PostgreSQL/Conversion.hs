{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.PostgreSQL.Conversion () where
{- Providing Convertible instances to use with toSql/fromSql. -}

import Database.HDBC
import Data.Convertible.Base

import OpenBrain.Data as Data
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Id   (Id)
import OpenBrain.Data.Salt (Salt)
import qualified OpenBrain.Data.Hash as Hash
import qualified OpenBrain.Data.Id   as Id
import qualified OpenBrain.Data.Salt as Salt

instance Convertible Hash SqlValue where
  safeConvert = Right . toSql . Hash.fromHash
instance Convertible SqlValue Hash where
  safeConvert = Right . Hash.toHash . fromSql

instance Convertible Id SqlValue where
  safeConvert = Right . toSql . Id.unwrap
instance Convertible SqlValue Id where
  safeConvert = Right . Id.wrap . fromSql

instance Convertible RelationType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue RelationType where
  safeConvert = Right . toEnum . fromSql

instance Convertible Salt SqlValue where
  safeConvert = Right . toSql . Salt.fromSalt
instance Convertible SqlValue Salt where
  safeConvert = Right . Salt.toSalt . fromSql
