{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Happstack.Server as S

import OpenBrain.Backend (CBackend, CUserBackend, CKarmaBackend, CSaltShaker)
import qualified OpenBrain.Backend as B
import OpenBrain.Common
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash, hash)
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt (Salt, mkSalt)
import OpenBrain.Website.Action.Common (failMessage, successMessage, handleFail)
import OpenBrain.Website.Common hiding (handleFail)
import OpenBrain.Website.Monad
import OpenBrain.Website.Session

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
  lift    $ mkSession b $ userid userData
  successMessage "Creation complete."

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
  lift $ mkSession b uid
  successMessage "Login complete."

logout :: OBW Response
logout = do
  b <- gets backend
  lift $ dropSession b
  successMessage "Logout complete."

{- Expects parameters: username -}
delete :: OBW Response
delete = handleFail "Invalid session." $ do
  b           <- gets backend
  deletename  <- look "username"
  uid         <- liftMaybe =<< lift (chkSession b)
  handleFail "Data for deletion not found." $ do
    userData <- liftMaybeT $ B.getUser b uid
    deleteId <- liftMaybeT $ B.hasUserWithName b deletename
    handleFail "Not allowed to delete user." $ do
      let isA = isAdmin userData
      let isSelf = deleteId == userid userData
      guard $ isA || isSelf
      liftIO $ B.delete b deleteId
      when isSelf $ lift $ dropSession b
      successMessage "Deleted requested user."

{-
  Expects parameters: userid, karma :: Int
  Clients can add or sub other clients karma
  as long as they pay the same amount.
-}
changeKarma :: OBW Response
changeKarma = handleFail "Invalid session." $ do
  b       <- gets backend
  uid     <- liftMaybe =<< lift (chkAction b)
  target  <- liftM (toId . read) $ look "userid"
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
      successMessage "Updated karma."
  where
    karmaUpdate :: Int -> Karma -> Karma
    karmaUpdate x y
      | x < 0     = (y -) . toKarma $ negate x
      | otherwise = (y +) $ toKarma x

{- Expects parameters: username, password -}
changePwd :: OBW Response
changePwd = handleFail "Invalid session." $ do
  b         <- gets backend
  uid       <- liftMaybe =<< lift (chkAction b)
  username  <- look "username"
  password  <- look "password"
  isAdmin   <- liftM isAdmin . liftMaybeT  $ B.getUser b uid
  isSelf    <- liftM (uid ==) . liftMaybeT $ B.hasUserWithName b username
  handleFail "Can't update password of another user." $ do
    guard $ isAdmin || isSelf
    target  <- liftMaybeT $ B.hasUserWithName b username
    salt    <- liftIO $ B.getSalt b target
    liftIO $ B.updatePasswd b target $ hash salt password
    successMessage "Password changed."

{- Expects parameters: username, admin :: {1,0} -}
admin :: OBW Response
admin = handleFail "Invalid session." $ do
  setA      <- liftM (=="1") $ look "admin"
  b         <- gets backend
  uid       <- liftMaybe =<< lift (chkAction b)
  isA       <- liftM isAdmin . liftMaybeT $ B.getUser b uid
  handleFail "You need to be admin for this." $ do
    guard isA
    username <- look "username"
    handleFail "Username doesn't exist." $ do
      target <- liftMaybeT $ B.hasUserWithName b username
      liftIO $ B.setAdmin b target setA
      successMessage "Changed admin status."

