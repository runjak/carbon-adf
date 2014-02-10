{-# LANGUAGE GADTs #-}
module Carbon.Backend.PostgreSQL (load, validConfig) where

import Control.Monad
import Data.Maybe
import Database.HDBC.PostgreSQL as PSQL
import qualified Database.HDBC as HDBC

import Carbon.Backend.DSL as DSL
import Carbon.Backend.PostgreSQL.Common (Connector, Executor, exec)
import Carbon.Config as Config
import qualified Carbon.Backend.PostgreSQL.AcceptanceCondition as AcceptanceCondition
import qualified Carbon.Backend.PostgreSQL.Common as Common
import qualified Carbon.Backend.PostgreSQL.Discussion as Discussion
import qualified Carbon.Backend.PostgreSQL.Item as Item
import qualified Carbon.Backend.PostgreSQL.Paging as Paging
import qualified Carbon.Backend.PostgreSQL.User as User

load :: Config -> IO CBackendProcessor
load = liftM CBackendProcessor . Common.load

validConfig :: Config -> Bool
validConfig c = case backendType c of
                  (PostgreSQLBackend _) -> True
                  _ -> False

instance BackendProcessor Connector where
  process c e = flip process e =<< Common.connect c

instance BackendProcessor Executor where
  process e (Backendλ m ma) = process e . ma =<< process e m
  process e (Nop         r) = return r
  process _ (BackendIO io ) = io
  -- | User related:
  process e (AddUser uname hs isA) = exec e $ User.addUser uname hs isA
  process e (DeleteUser  uid heir) = exec e $ User.deleteUser uid heir
  process e  GetNobody             = exec e User.getNobody
  process e (GetUser          uid) = exec e $ User.getUser uid
  process e (HasUser        uname) = exec e $ User.hasUser uname
  process e (Login          uid f) = exec e $ User.login uid f
  process e (Validate    uid skey) = exec e $ User.validate uid skey
  process e (Logout           uid) = exec e $ User.logout uid
  process e (SetAdmin     uid isA) = exec e $ User.setAdmin uid isA
  process e (SetPasswd      uid f) = exec e $ User.setPasswd uid f
  process e (SetProfile  uid mAid) = exec e $ User.setProfile uid mAid
  -- | Item related:
  process e (AddItem uid)    = exec e $ Item.addItem uid
  process e (Clone iid)      = exec e $ Item.clone iid
  process e (GetItem iid)    = exec e $ Item.getItem iid
  process e (IdFromHeadline h) = exec e $ Item.idFromHeadline h
  process e (SetItem i)      = exec e $ Item.setItem i
  process e (DeleteItem iid) = exec e $ Item.deleteItem iid
  process e (ModifyAcceptanceCondition i) = exec e $ AcceptanceCondition.setCondition' i
  -- | Discussion related:
  process e (Evaluate iid) = exec e $ Discussion.evaluate iid
  -- | Paging related:
  process e (ItemCount p)   = exec e $ Paging.itemCount p
  process e (PageItems p)   = exec e $ Paging.pageItems p
  process e UserCount       = exec e Paging.userCount
  process e (PageUsers l o) = exec e $ Paging.pageUsers l o
  -- | Logging:
  process e (LogString s) = putStrLn $ "BackendDSL.Logstring:\t"++s
