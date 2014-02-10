{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Carbon.Backend.PostgreSQL.Conversion () where
{- Providing Convertible instances to use with toSql/fromSql. -}

import Database.HDBC
import Data.Convertible.Base

import Carbon.Data as Data
import Carbon.Data.Hash (Hash)
import Carbon.Data.Id   (Id)
import Carbon.Data.Salt (Salt)
import qualified Carbon.Data.Hash as Hash
import qualified Carbon.Data.Id   as Id
import qualified Carbon.Data.Salt as Salt

instance Convertible Hash SqlValue where
  safeConvert = Right . toSql . Hash.fromHash
instance Convertible SqlValue Hash where
  safeConvert = Right . Hash.toHash . fromSql

instance Convertible Id SqlValue where
  safeConvert = Right . toSql . Id.unwrap
instance Convertible SqlValue Id where
  safeConvert = Right . Id.wrap . fromSql

instance Convertible Salt SqlValue where
  safeConvert = Right . toSql . Salt.fromSalt
instance Convertible SqlValue Salt where
  safeConvert = Right . Salt.toSalt . fromSql

instance Convertible ResultType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue ResultType where
  safeConvert = Right . toEnum . fromSql

instance Convertible (Exp Id) SqlValue where
  safeConvert = Right . toSql . show
instance Convertible SqlValue (Exp Id) where
  safeConvert s = let s' = fromSql s
                  in go $ execParser' parseExp "PostgreSQL representation" s'
                  where
                    go :: Either String (Exp String) -> ConvertResult (Exp Id)
                    go (Right e) = Right $ fmap read e
                    go (Left  e) = Left ConvertError {
                        convSourceValue  = show s
                      , convSourceType   = "SqlValue"
                      , convDestType     = "Exp Id"
                      , convErrorMessage = e
                      }

instance Convertible ProofStandard SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue ProofStandard where
  safeConvert = Right . toEnum . fromSql

instance Convertible RelationType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue RelationType where
  safeConvert = Right . toEnum . fromSql

instance Convertible EvaluationState SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue EvaluationState where
  safeConvert = Right . toEnum . fromSql

instance Convertible [ResultType] SqlValue where
  safeConvert = Right . toSql . show
instance Convertible SqlValue [ResultType] where
  safeConvert = Right . read . fromSql

instance Convertible ResultState SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue ResultState where
  safeConvert = Right . toEnum . fromSql
