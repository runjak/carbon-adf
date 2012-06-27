{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Happstack.Server as S

import OpenBrain.Backend (CBackend, CUserBackend, CKarmaBackend, CSaltShaker)
import qualified OpenBrain.Backend as B
import OpenBrain.Common
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash, hash)
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt (Salt, mkSalt)
import OpenBrain.Website.Action.Common (failMessage, successMessage)
import OpenBrain.Website.Common
import OpenBrain.Website.Session

serve :: CBackend -> ServerPartT IO Response
serve b = msum [
    dir "create"   $ create       b
  , dir "login"    $ login        b
  , dir "logout"   $ logout       b
  , dir "delete"   $ delete       b
  , dir "karma"    $ changeKarma  b
  , dir "password" $ changePwd    b
  , dir "admin"    $ admin        b
  ]

{-
  Expects parameters: username, password
-}
create :: CBackend -> ServerPartT IO Response
create b = do
  username <- look "username"
  salt <- liftIO mkSalt
  hash <- liftM (hash salt) $ look "password"
  liftIO $ putStrLn $ "Request for creation: " ++ username ++ " with hash: " ++ show hash
  mUserData <- liftIOMay $ B.register (B.userBackend b) username hash
  case mUserData of
    Nothing -> failMessage "Could not register user."
    (Just userdata) -> do
      liftIO $ B.setId (B.saltShaker b) salt $ userid userdata
      mkSession b $ userid userdata
      successMessage "Creation complete."

{-
  Expects parameters: username, password
-}
login :: CBackend -> ServerPartT IO Response
login b = do
  username <- look "username"
  mUid <- liftIOMay $ B.hasUserWithName (B.userBackend b) username
  case mUid of
    Nothing -> failMessage "Login failed."
    (Just uid) -> do
      salt <- liftIO $ B.getSalt (B.saltShaker b) uid
      hash <- liftM (hash salt) $ look "password"
      mUserData <- liftIOMay $ B.login (B.userBackend b) username hash
      case mUserData of
        Nothing -> failMessage "Login failed."
        (Just userdata) -> do
          let uid = userid userdata
          mkSession b uid
          successMessage "Login complete."

logout :: CBackend -> ServerPartT IO Response
logout b = dropSession b >> successMessage "Logout complete."

{- Expects parameters: username -}
delete :: CBackend -> ServerPartT IO Response
delete b = do
  deletename <- look "username"
  muid <- chkSession b
  case muid of
    Nothing -> (liftIO $ putStrLn "delete #1") >> failMessage "Invalid session"
    (Just uid) -> do
      mUser <- liftIOMay $ B.getUser (B.userBackend b) uid
      mDeleteId <- liftIOMay $ B.hasUserWithName (B.userBackend b) deletename
      let mUserDeleteId = liftM2 (,) mUser mDeleteId
      case mUserDeleteId of
        Nothing -> failMessage "Data for deletion not found"
        Just (userData, deleteId) -> do
          deleteKarma <- liftIO . B.karmaDeleteUser $ B.karmaBackend b
          let allowed = or [userid userData == deleteId, isAdmin userData, karma userData >= deleteKarma]
          guard allowed
          success <- liftIO $ B.delete (B.userBackend b) deleteId
          dropSession b
          successMessage "Deleted requested user."
          `mplus` (failMessage "Not allowed to delete requested user.")

{-
  Expects parameters: userid, karma :: Int
  Clients can add or sub other clients karma
  as long as they pay the same amount.
-}
changeKarma :: CBackend -> ServerPartT IO Response
changeKarma b = do
  muid <- chkAction b
  case muid of
    Nothing -> failMessage "Invalid session"
    (Just uid) -> do
      userid <- liftM (toId . read) $ look "userid"
      k <- lookRead "karma"
      let change  = karmaUpdate k
      let burn    = karmaUpdate . negate $ abs k
      userdata <- liftM fromJust . liftIOMay $ B.getUser (B.userBackend b) uid
      let preds = [(userid == uid, "Can't give yourself karma.")
                , ((fromKarma $ karma userdata) < abs k, "Not enough karma.")]
      case (map snd $ filter fst preds) of
        (m:_) -> failMessage m
        [] -> do
          -- Change someones karma:
          liftIO $ B.updateKarma (B.userBackend b) userid change
          -- Delete from clients karma:
          liftIO $ B.updateKarma (B.userBackend b) uid burn
          successMessage "Updated karma."
  where
    karmaUpdate :: Int -> Karma -> Karma
    karmaUpdate x y
      | x < 0     = (y -) . toKarma $ negate x
      | otherwise = (y +) $ toKarma x

{- Expects parameters: username, password -}
changePwd :: CBackend -> ServerPartT IO Response
changePwd b = do
  muid <- chkAction b
  case muid of
    Nothing -> failMessage "Invalid session"
    (Just uid) -> do
      username <- look "username"
      password <- look "password"
      admin <- liftM (maybe False $ isAdmin) . liftIOMay  $ B.getUser (B.userBackend b) uid
      self  <- liftM (maybe False $ (==) uid) . liftIOMay $ B.hasUserWithName (B.userBackend b) username
      case admin || self of
        False -> failMessage "Can't update password of another user."
        True -> do
          liftIO $ do
            target  <- liftM fromJust . runMaybeT $ B.hasUserWithName (B.userBackend b) username
            salt    <- B.getSalt (B.saltShaker b) target
            B.updatePasswd (B.userBackend b) target $ hash salt password
          successMessage "Password changed."

{- Expects parameters: username, admin :: {1,0} -}
admin :: CBackend -> ServerPartT IO Response
admin b = do
  muid <- chkAction b
  case muid of
    Nothing -> failMessage "Invalid session"
    (Just uid) -> do
      username <- look "username"
      setA <- liftM (=="1") $ look "admin"
      isA <- liftM (maybe False $ isAdmin) . liftIOMay $ B.getUser (B.userBackend b) uid
      mTarget <- liftIOMay $ B.hasUserWithName (B.userBackend b) username
      case isA && isJust mTarget of
        False -> failMessage "Cannot change status, not admin yourself or username doesn't exist."
        True -> do
          liftIO $ B.setAdmin (B.userBackend b) (fromJust mTarget) isA
          successMessage "Changed admin status."

