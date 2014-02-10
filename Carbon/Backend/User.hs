module Carbon.Backend.User where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Maybe as Maybe

import Carbon.Backend.DSL
import Carbon.Data
import qualified Carbon.Data.Hash as Hash
import qualified Carbon.Data.Salt as Salt

{-|
  This function ensures that an admin user and Nobody exist.
|-}
init :: BackendDSL ()
init = do
  -- Making sure Nobody exists:
  GetNobody
  -- Making sure we've got an admin user:
  mAdmin <- HasUser "admin"
  when (Maybe.isNothing mAdmin) $ do
    liftIO $ putStrLn "Adding user admin:admin…"
    salt <- liftIO Salt.mkSalt
    let hash = Hash.hash salt "admin"
        no = error . ("Could not create admin user in Carbon.Backend.User:init:\n"++)
        go = const $ return ()
    either no go =<< AddUser "admin" (hash, salt) True
