{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Common
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash, hash)
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt (Salt, mkSalt)
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session
import qualified OpenBrain.Website.Html.Login as Login
import qualified OpenBrain.Backend.Monad as OBB

serve :: OBW Response
serve = msum [
    dir "create"    create
  , dir "login"     login
  , dir "logout"    logout
  , dir "delete"    delete
  , dir "karma"     changeKarma
  , dir "password"  changePwd
  , dir "admin"     admin
  ]

{-
  Expects parameters: username, password
-}
create :: OBW Response
create = handleFail "Could not register user." $ do
  username  <- look "username"
  guard $ username /= "Nobody"
  salt      <- liftIO mkSalt
  hash      <- liftM (hash salt) $ look "password"
  userData  <- liftOBB $ OBB.register username hash salt
  mkSession $ userid userData
  ok . toResponse =<< Login.logoutBox (userid userData)

{-
  Expects parameters: username, password
-}
login :: OBW Response
login = handleFail "Login failed." $ do
  username  <- look "username"
  password  <- look "password"
  uid       <- liftOBB $ do
    uid <- OBB.hasUserWithName username
    salt <- OBB.getSalt uid
    OBB.login username $ hash salt password
    return uid
  mkSession uid
  ok . toResponse =<< Login.logoutBox uid

logout :: OBW Response
logout = do
  dropSession
  ok . toResponse =<< Login.loginBox

{- Expects parameters: username -}
delete :: OBW Response
delete = handleFail "Invalid session." $ do
  deletename  <- look "username"
  uid         <- chkSession
  handleFail "Data for deletion not found." $ do
    userData <- liftOBB $ OBB.getUser uid
    deleteId <- liftOBB $ OBB.hasUserWithName deletename
    handleFail "Not allowed to delete user." $ do
      let isA = isAdmin userData
      let isSelf = deleteId == userid userData
      guard $ isA || isSelf
      liftOBB $ OBB.delete' deleteId
      when isSelf dropSession
      handleSuccess "Deleted requested user."

{-
  Expects parameters: userid, karma :: Int
  Clients can add or sub other clients karma
  as long as they pay the same amount.
-}
changeKarma :: OBW Response
changeKarma = handleFail "Invalid session." $ do
  uid     <- chkSession
  target  <- liftM (fromId . wrap . read) $ look "userid"
  k       <- lookRead "karma"
  let change  = karmaUpdate k
  let burn    = karmaUpdate . negate $ abs k
  userdata <- liftOBB $ OBB.getUser uid
  handleFail "Can't give yourself karma." $ do
    guard $ target /= uid
    handleFail "Not enough karma." $ do
      guard $ abs k <= fromKarma (karma userdata)
      -- Change someones karma:
      liftOBB $ OBB.updateKarma target change
      -- Delete from clients karma:
      liftOBB $ OBB.updateKarma uid burn
      handleSuccess "Updated karma."
  where
    karmaUpdate :: Int -> Karma -> Karma
    karmaUpdate x y
      | x < 0     = (y -) . toKarma $ negate x
      | otherwise = (y +) $ toKarma x

{- Expects parameters: username, password -}
changePwd :: OBW Response
changePwd = handleFail "Invalid session." $ do
  uid       <- chkSession
  username  <- look "username"
  password  <- look "password"
  isAdmin   <- liftM isAdmin . liftOBB  $ OBB.getUser uid
  isSelf    <- liftM (uid ==) . liftOBB $ OBB.hasUserWithName username
  handleFail "Can't update password of another user." $ do
    guard $ isAdmin || isSelf
    target  <- liftOBB $ OBB.hasUserWithName username
    salt    <- liftOBB $ OBB.getSalt target
    liftOBB . OBB.updatePasswd target $ hash salt password
    handleSuccess "Password changed."

{- Expects parameters: username, admin :: {1,0} -}
admin :: OBW Response
admin = handleFail "Invalid session." $ do
  setA  <- liftM (=="1") $ look "admin"
  uid   <- chkSession
  isA   <- liftM isAdmin . liftOBB $ OBB.getUser uid
  handleFail "You need to be admin for this." $ do
    guard isA
    username <- look "username"
    handleFail "Username doesn't exist." $ do
      target <- liftOBB $ OBB.hasUserWithName username
      liftOBB $ OBB.setAdmin target setA
      handleSuccess "Changed admin status."

