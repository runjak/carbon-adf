{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.SqliteBackend.Schema (
    initTables
  , tables
  , SnippetType(..)
) where

import Data.Convertible.Base as DBC
import Database.HDBC as H

initTables :: [String]
initTables = map ("CREATE TABLE IF NOT EXISTS "++) tables

tables :: [String]
tables = [
    "UserData ("
  ++"    userid    SERIAL"
  ++"  , username  VARCHAR(255)  NOT NULL"
  ++"  , password  VARCHAR(255)  NOT NULL"
  ++"  , karma     INT UNSIGNED  NOT NULL DEFAULT 0"
  ++"  , creation  INTEGER       NOT NULL"
  ++"  , lastLogin INTEGER       NOT NULL"
  ++"  , isAdmin   TINYINT(1)    NOT NULL DEFAULT 0"
  ++")"
  , "Profile ("
  ++"    profileid SERIAL"
  ++"  , userid    BIGINT(20) NOT NULL"
  ++"  , avatar    TEXT"
  ++"  , FOREIGN KEY(userid) REFERENCES UserData(userid)"
  ++")"
  , "Name ("
  ++"    profileid   BIGINT(20)   UNIQUE NOT NULL"
  ++"  , prefix      VARCHAR(255) NOT NULL DEFAULT ''"
  ++"  , foreName    VARCHAR(255) NOT NULL DEFAULT ''"
  ++"  , middleName  VARCHAR(255) NOT NULL DEFAULT ''"
  ++"  , familyName  VARCHAR(255) NOT NULL DEFAULT ''"
  ++"  , suffix      VARCHAR(255) NOT NULL DEFAULT ''"
  ++"  , FOREIGN KEY(profileid) REFERENCES Profile(profileid)"
  ++")"
  , "Location ("
  ++"    profileid BIGINT(20)    NOT NULL"
  ++"  , city      VARCHAR(255)  NOT NULL DEFAULT ''"
  ++"  , state     VARCHAR(255)  NOT NULL DEFAULT ''"
  ++"  , land      VARCHAR(255)  NOT NULL DEFAULT ''"
  ++"  , zipCode   VARCHAR(255)  NOT NULL DEFAULT ''"
  ++"  , note      TEXT          NOT NULL DEFAULT ''"
  ++"  , FOREIGN KEY(profileid) REFERENCES Profile(profileid)"
  ++")"
  , "ProfileSnippet ("
  ++"    profileid   BIGINT(20)    NOT NULL"
  ++"  , title       VARCHAR(255)  NOT NULL DEFAULT ''"
  ++"  , description TEXT          NOT NULL DEFAULT ''"
  ++"  , target      TEXT          NOT NULL DEFAULT ''"
  ++"  , snippetType TINYINT NOT NULL" -- use data SnippetType
  ++"  , FOREIGN KEY(profileid) REFERENCES Profile(profileid)"
  ++")"
  ]

-- Must have it's instances here because it should be defined with the Schema.
data SnippetType = Website | Email | InstantMessager
  deriving (Show, Read, Eq, Ord, Enum)
instance Convertible SnippetType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue SnippetType where
  safeConvert = Right . toEnum . fromSql
