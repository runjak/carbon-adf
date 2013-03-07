{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Website.Common hiding (delete)
import OpenBrain.Website.Session
import qualified OpenBrain.Backend as Backend

serve :: OBW Response
serve = msum [
    dir "create"   create
  , dir "login"    login
  , dir "logout"   logout
  , dir "delete"   delete
  , dir "karma"    changeKarma
  , dir "password" changePwd
  , dir "admin"    admin
  ]

{-
  Expects parameters: username, password
-}
create :: OBW Response
create = handleFail "Missing parameter[s], need: [username, password]" $ do
  username <- look "username"
  password <- look "password"
  handleFail "Could not register." $ do
    guard $ username /= "Nobody"
    salt <- liftIO mkSalt
    let h = hash salt password
    userData <- noMaybe . liftOBB $ Register username h salt
    mkSession $ userid userData
    jsonSuccess' "Successfully registered." userData

{-
  Expects parameters: username, password
-}
login :: OBW Response
login = handleFail "Missing parameter[s], need: [username, password]" $ do
  username <- look "username"
  password <- look "password"
  handleFail "Incorrect Login/Password." $ do
    muid <- liftOBB $ HasUserWithName username
    guard $ isJust muid
    salt <- liftOBB . GetSalt $ fromJust muid
    mud  <- liftOBB . Login username $ hash salt password
    guard $ isJust mud
    jsonSuccess' "Login successfull." $ fromJust mud

logout :: OBW Response
logout = dropSession >> jsonSuccess "Logged out."

{- Expects parameters: username -}
delete :: OBW Response
delete = handleFail "Invalid session." $ do
  deletename <- look "username"
  uid        <- chkSession
  handleFail "Data for deletion not found." $ do
    userData <- noMaybe . liftOBB $ GetUser uid
    deleteId <- noMaybe . liftOBB $ HasUserWithName deletename
    handleFail "Not allowed to delete user." $ do
      let isA = isAdmin userData
      let isSelf = deleteId == userid userData
      guard $ isA || isSelf
      liftOBB $ delete' deleteId
      when isSelf dropSession
      jsonSuccess "Deleted requested user."

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
  userdata <- noMaybe . liftOBB $ GetUser uid
  handleFail "Can't give yourself karma." $ do
    guard $ target /= uid
    handleFail "Not enough karma." $ do
      guard $ abs k <= fromKarma (karma userdata)
      -- Change someones karma:
      liftOBB $ UpdateKarma target change
      -- Delete from clients karma:
      liftOBB $ UpdateKarma uid burn
      jsonSuccess "Updated karma."
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
  isAdmin   <- liftM isAdmin . noMaybe . liftOBB  $ GetUser uid
  isSelf    <- liftM (uid ==) . noMaybe . liftOBB $ HasUserWithName username
  handleFail "Can't update password of another user." $ do
    guard $ isAdmin || isSelf
    target  <- noMaybe . liftOBB $ HasUserWithName username
    salt    <- liftOBB $ GetSalt target
    liftOBB . UpdatePasswd target $ hash salt password
    jsonSuccess "Password changed."

{- Expects parameters: username, admin :: {1,0} -}
admin :: OBW Response
admin = handleFail "Invalid session." $ do
  setA  <- liftM (=="1") $ look "admin"
  uid   <- chkSession
  isA   <- liftM isAdmin . noMaybe . liftOBB $ GetUser uid
  handleFail "You need to be admin for this." $ do
    guard isA
    username <- look "username"
    handleFail "Username doesn't exist." $ do
      target <- noMaybe . liftOBB $ HasUserWithName username
      liftOBB $ SetAdmin target setA
      jsonSuccess "Changed admin status."

