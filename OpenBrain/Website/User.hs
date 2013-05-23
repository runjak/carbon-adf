{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.User where

import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Salt (Salt)
import OpenBrain.Website.Common
import qualified OpenBrain.Data.Hash as Hash
import qualified OpenBrain.Data.Salt as Salt
import qualified OpenBrain.Website.Session as Session

createUser :: OBW Response
createUser = plusm badReq $ do
  -- | Necessary data to create a user:
  uname <- getUsername
  salt  <- liftIO Salt.mkSalt
  hash  <- getPassword salt
  isA   <- getIsAdmin
  -- | Checking for duplicates:
  mUid  <- liftB $ AddUser uname (hash,salt) isA
  case mUid of
    (Just uid) -> respCreated $ responseJSON' uid
    Nothing -> respInternalServerError $ responseJSON "Could not create User."
  where
    badReq = respBadRequest $ responseJSON "Expected parameters are: [username, password, isAdmin]"

pageUsers :: OBW Response
pageUsers = countAndPageBy UserCount $ \l o -> liftM responseJSON' $ PageUsers l o

readUser :: UserId -> OBW Response
readUser = respOk . responseJSON' <=< liftB . GetUser

updateUser :: UserId -> OBW Response
updateUser target = do
  -- | Checking permissions:
  u <- liftB . GetUser =<< Session.chkSession
  plusm permFail $ do
    guard (isAdmin u || (userId u == target))
    -- | Possible parameters:
    mPass <- getPassword'
    mIsAd <- getIsAdmin'
    mProf <- getProfile
    -- Performing the requested updates:
    sequence_ [
        nop (liftB . SetPasswd target ) mPass
      , nop (liftB . SetAdmin target  ) mIsAd
      , nop (liftB . SetProfile target) mProf
      ]
    -- | Present the modified user:
    readUser target
  where
    permFail = respForbidden $ responseJSON "You must be logged in as admin or the user itself to perform updates."
    -- | Tries to apply a Maybe to a function in case of Just or performs a nop
    nop = maybe $ return ()

deleteUser :: UserId -> OBW Response
deleteUser target = do
  -- | Checking permissions:
  u <- liftB . GetUser =<< Session.chkSession
  plusm permFail $ do
    guard (isAdmin u || (userId u == target))
    -- | Deleting user:
    liftB $ do
      heir <- GetNobody
      DeleteUser target heir
    -- | Success messsage:
    respOk . responseJSON' $ "Deleted user("++show target++")."
  where
    permFail = respForbidden $ responseJSON "You must be logged in as admin or the user itself to delete a user."

-- Parameters…
getUsername :: OBW Username
getUsername = liftM sanitize $ look "username"

getPassword :: Salt -> OBW Hash
getPassword salt = liftM (Hash.hash salt) $ look "password"

getPassword' :: OBW (Maybe (Salt -> Hash))
getPassword' = plusm (return Nothing) $ do
  p <- look "password"
  return . Just $ flip Hash.hash p

getIsAdmin :: OBW Bool
getIsAdmin = lookRead "isAdmin" 

getIsAdmin' :: OBW (Maybe Bool)
getIsAdmin' = msum [liftM Just getIsAdmin, return Nothing]

{-
  The inner maybe tells if an article should be set as the profile or if the profile is to be come a NULL value.
  The outer maybe determines if the profile should be changed.
-}
getProfile :: OBW (Maybe (Maybe ArticleId))
getProfile = msum [emptyProfile, someProfile, noProfile]
  where
    emptyProfile = do
      pIsNothing <- liftM (== "Nothing") $ look "profile"
      guard pIsNothing
      return $ Just Nothing
    someProfile  = liftM (Just . Just) $ lookRead "profile"
    noProfile    = return Nothing
