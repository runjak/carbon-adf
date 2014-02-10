{-# LANGUAGE ExistentialQuantification, GADTs #-}
module Carbon.Backend.DSL where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Set (Set)

import Carbon.Data

{-| The BackendDSL and it's verbs: |-}
data BackendDSL r where
  -- Composition:
  Backendλ :: BackendDSL p -> (p -> BackendDSL r) -> BackendDSL r
  Nop :: r -> BackendDSL r
  -- Perform IO:
  BackendIO :: IO a -> BackendDSL a
  -- User:
  AddUser :: Username -> (Hash, Salt) -> IsAdmin -> BackendDSL (Either Error UserId)
  DeleteUser :: UserId -> Heir -> BackendDSL (Maybe Error)
  GetNobody :: BackendDSL UserId
  GetUser :: UserId -> BackendDSL (Either Error User)
  HasUser :: Username -> BackendDSL (Maybe UserId)
  Login :: UserId -> (Salt -> Hash) -> BackendDSL (Maybe SessionKey)
  Validate :: UserId -> SessionKey -> BackendDSL Bool
  Logout :: UserId -> BackendDSL ()
  SetAdmin :: UserId -> IsAdmin -> BackendDSL ()
  SetPasswd :: UserId -> (Salt -> Hash) -> BackendDSL ()
  SetProfile :: UserId -> Maybe ItemId -> BackendDSL ()
  -- Item:
  AddItem :: UserId -> BackendDSL (Item Id)
  Clone :: ItemId -> BackendDSL (Either Error (Item Id))
  GetItem :: ItemId -> BackendDSL (Either Error (Item Id))
  IdFromHeadline :: String -> BackendDSL (Maybe ItemId)
  SetItem :: Item Id -> BackendDSL (Either Error (Item Id))
  DeleteItem :: ItemId -> BackendDSL (Maybe Error)
  ModifyAcceptanceCondition :: Item Id -> BackendDSL () -- This only changes an item's ac
  -- Discussion:
  Evaluate :: ItemId -> BackendDSL EvaluationState
  -- Paging:
  ItemCount :: Paging -> BackendDSL Count
  PageItems :: Paging -> BackendDSL [ItemId]
  UserCount :: BackendDSL Count
  PageUsers :: Limit -> Offset -> BackendDSL [UserId]
  -- Logging:
  LogString :: String -> BackendDSL ()

{-| The Monad instance for BackendDSL to enable beautiful composition. |-}
instance Monad BackendDSL where
  (>>=)  = Backendλ
  return = Nop

{-| Allowing the BackendDSL to perform IO operations. |-}
instance MonadIO BackendDSL where
  liftIO = BackendIO

{-|
  The BackendProcessor is build by the module Carbon.Backend.Load from a given Config file.
  It ensures, that the rest of the application will only use the BackendDSL to communicate with the Backend.
  Because a BackendDSL statement can only be executed by the process function using a BackendProcessor,
  the Backend can be exchanged nicely.
|-}
class BackendProcessor b where
  process :: b -> BackendDSL r -> IO r

{-|
  CBackendProcessor is a simple Container for everything that is a BackendProcessor.
  This makes it possible to pass BackendProcessors around without having to deal
  with it's specific type, because that is hidden in the Container.
  Of course the CBackendProcessor is itself a BackendProcessor and
  thereby allowes us to process BackendDSL statements directly and we don't need
  a method to extract the original BackendProcessor.
  Also note, that it would be possible to easily inject a BackendProcessor
  for debugging purposes.
|-}
data CBackendProcessor = forall b . BackendProcessor b => CBackendProcessor b
instance BackendProcessor CBackendProcessor where
  process (CBackendProcessor b) = process b
