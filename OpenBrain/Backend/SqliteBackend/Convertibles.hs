{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.SqliteBackend.Convertibles () where
{- Providing Convertible instances to use with toSql/fromSql. -}

import Database.HDBC
import Data.Convertible.Base

import OpenBrain.Data.Hash (Hash, toHash, fromHash)
import OpenBrain.Data.Id (Id, toId, fromId)
import OpenBrain.Data.Karma (Karma, toKarma, fromKarma)
import OpenBrain.Data.Salt (Salt, toSalt, fromSalt)

-- Instances to enable Id <-> SqlValue conversions:
instance Convertible Id SqlValue where
  safeConvert = Right . toSql. fromId
instance Convertible SqlValue Id where
  safeConvert = Right . toId . fromSql

-- Instances to enable Hash <-> SqlValue conversions:
instance Convertible Hash SqlValue where
  safeConvert = Right . toSql . fromHash
instance Convertible SqlValue Hash where
  safeConvert = Right . toHash . fromSql

-- Instances to enable Karma <-> SqlValue conversions:
instance Convertible Karma SqlValue where
  safeConvert = Right . toSql . fromKarma
instance Convertible SqlValue Karma where
  safeConvert = Right . toKarma . fromSql

-- Instances to enable Salt <-> SqlValue conversions:
instance Convertible Salt SqlValue where
  safeConvert = Right . toSql . fromSalt
instance Convertible SqlValue Salt where
  safeConvert = Right . toSalt . fromSql
