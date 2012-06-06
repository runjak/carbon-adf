{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.SqliteBackend.Convertibles () where
{- Providing Convertible instances to use with toSql/fromSql. -}

import Database.HDBC
import Data.Convertible.Base

import OpenBrain.User.Data as D
import OpenBrain.User.Hash (Hash, toString, fromString)
import OpenBrain.User.Karma (Karma, toKarma, fromKarma)

-- Instances to enable UserId <-> SqlValue conversions:
instance Convertible UserId SqlValue where
  safeConvert = Right . toSql. fromUserId
instance Convertible SqlValue UserId where
  safeConvert = Right . toUserId . fromSql

-- Instances to enable Hash <-> SqlValue conversions:
instance Convertible Hash SqlValue where
  safeConvert = Right . toSql . toString
instance Convertible SqlValue Hash where
  safeConvert = Right . fromString . fromSql

-- Instances to enable Karma <-> SqlValue conversions:
instance Convertible Karma SqlValue where
  safeConvert = Right . toSql . fromKarma
instance Convertible SqlValue Karma where
  safeConvert = Right . toKarma . fromSql
