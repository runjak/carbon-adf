{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.PostgreSQLBackend.Convertibles () where
{- Providing Convertible instances to use with toSql/fromSql. -}

import Database.HDBC
import Data.Convertible.Base

import OpenBrain.Data.Hash (Hash, toHash, fromHash)
import OpenBrain.Data.Id (Id, wrap, unwrap)
import OpenBrain.Data.Information (CollectionType)
import OpenBrain.Data.Karma (Karma, toKarma, fromKarma)
import OpenBrain.Data.Relation (RelationType)
import OpenBrain.Data.Salt (Salt, toSalt, fromSalt)

-- Instances to enable IdType <-> SqlValue conversions:
instance Convertible Id SqlValue where
  safeConvert = Right . toSql  . unwrap
instance Convertible SqlValue Id where
  safeConvert = Right . wrap . fromSql

-- Instances to enable CollectionType <-> SqlValue conversions:
instance Convertible CollectionType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue CollectionType where
  safeConvert = Right . toEnum . fromSql

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

-- Instances to enable RelationType <-> SqlValue conversions:
instance Convertible RelationType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue RelationType where
  safeConvert = Right . toEnum . fromSql
