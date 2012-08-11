{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Backend (CBackend, CUserBackend, CKarmaBackend, CSaltShaker)
import OpenBrain.Common
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash, hash)
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt (Salt, mkSalt)
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend as B
import qualified OpenBrain.Website.Session.Plus as Session

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
  salt      <- liftIO mkSalt
  hash      <- liftM (hash salt) $ look "password"
  b         <- gets backend
  userData  <- liftMaybeT $ B.register (B.userBackend b) username hash
  liftIO  $ B.setId (B.saltShaker b) salt $ userid userData
  Session.mkSession $ userid userData
  handleSuccess "Creation complete."

{-
  Expects parameters: username, password
-}
login :: OBW Response
login = handleFail "Login failed." $ do
  username  <- look "username"
  b         <- gets backend
  uid       <- liftMaybeT $ B.hasUserWithName b username
  salt      <- liftIO $ B.getSalt b uid
  hash      <- liftM (hash salt) $ look "password"
  userdata  <- liftMaybeT $ B.login (B.userBackend b) username hash
  Session.mkSession uid
  handleSuccess "Login complete."

logout :: OBW Response
logout = do
  b <- gets backend
  Session.dropSession
  handleSuccess "Logout complete."

{- Expects parameters: username -}
delete :: OBW Response
delete = handleFail "Invalid session." $ do
  b           <- gets backend
  deletename  <- look "username"
  uid         <- Session.chkSession
  handleFail "Data for deletion not found." $ do
    userData <- liftMaybeT $ B.getUser b uid
    deleteId <- liftMaybeT $ B.hasUserWithName b deletename
    handleFail "Not allowed to delete user." $ do
      let isA = isAdmin userData
      let isSelf = deleteId == userid userData
      guard $ isA || isSelf
      liftIO $ B.delete b deleteId
      when isSelf Session.dropSession
      handleSuccess "Deleted requested user."

{-
  Expects parameters: userid, karma :: Int
  Clients can add or sub other clients karma
  as long as they pay the same amount.
-}
changeKarma :: OBW Response
changeKarma = handleFail "Invalid session." $ do
  b       <- gets backend
  uid     <- Session.chkAction
  target  <- liftM (fromId . wrap . read) $ look "userid"
  k       <- lookRead "karma"
  let change  = karmaUpdate k
  let burn    = karmaUpdate . negate $ abs k
  userdata <- liftMaybeT $ B.getUser b uid
  handleFail "Can't give yourself karma." $ do
    guard $ target /= uid
    handleFail "Not enough karma." $ do
      guard $ abs k <= fromKarma (karma userdata)
      -- Change someones karma:
      liftIO $ B.updateKarma b target change
      -- Delete from clients karma:
      liftIO $ B.updateKarma b uid burn
      handleSuccess "Updated karma."
  where
    karmaUpdate :: Int -> Karma -> Karma
    karmaUpdate x y
      | x < 0     = (y -) . toKarma $ negate x
      | otherwise = (y +) $ toKarma x

{- Expects parameters: username, password -}
changePwd :: OBW Response
changePwd = handleFail "Invalid session." $ do
  b         <- gets backend
  uid       <- Session.chkAction
  username  <- look "username"
  password  <- look "password"
  isAdmin   <- liftM isAdmin . liftMaybeT  $ B.getUser b uid
  isSelf    <- liftM (uid ==) . liftMaybeT $ B.hasUserWithName b username
  handleFail "Can't update password of another user." $ do
    guard $ isAdmin || isSelf
    target  <- liftMaybeT $ B.hasUserWithName b username
    salt    <- liftIO $ B.getSalt b target
    liftIO $ B.updatePasswd b target $ hash salt password
    handleSuccess "Password changed."

{- Expects parameters: username, admin :: {1,0} -}
admin :: OBW Response
admin = handleFail "Invalid session." $ do
  setA      <- liftM (=="1") $ look "admin"
  b         <- gets backend
  uid       <- Session.chkAction
  isA       <- liftM isAdmin . liftMaybeT $ B.getUser b uid
  handleFail "You need to be admin for this." $ do
    guard isA
    username <- look "username"
    handleFail "Username doesn't exist." $ do
      target <- liftMaybeT $ B.hasUserWithName b username
      liftIO $ B.setAdmin b target setA
      handleSuccess "Changed admin status."

