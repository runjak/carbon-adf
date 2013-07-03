{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.User where

import Data.Maybe (fromJust)
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Salt (Salt)
import OpenBrain.Website.Common
import qualified OpenBrain.Data.Hash as Hash
import qualified OpenBrain.Data.Salt as Salt
import qualified OpenBrain.Website.Session as Session

createUser :: OBW Response
createUser = plusm badReq $ do
  liftIO $ putStrLn "OpenBrain.Website.User:createUser"
  -- | Necessary data to create a user:
  uname <- getUsername
  salt  <- liftIO Salt.mkSalt
  hash  <- getPassword salt
  isA   <- chkIsAdmin =<< getIsAdmin
  -- | Checking for duplicates:
  mUid  <- liftB $ AddUser uname (hash,salt) isA
  case mUid of
    (Just uid) -> do
      (sKey, u) <- liftB $ do
        mSkey <- Login uid $ const hash
        liftM ((,) $ fromJust mSkey) $ GetUser uid
      Session.mkSession uid sKey
      respCreated $ responseJSON' u 
    Nothing -> respInternalServerError $ responseJSON'' "Could not create User."
  where
    badReq = respBadRequest $ responseJSON'' "Expected parameters are: [username, password, isAdmin]"

pageUsers :: OBW Response
pageUsers = countAndPageBy UserCount $ \l o -> liftM responseJSON' $ PageUsers l o

readUser :: UserId -> OBW Response
readUser uid = respOk . responseJSON' =<< liftB (GetUser uid)

updateUser :: UserId -> OBW Response
updateUser target = do
  liftIO $ putStrLn "OpenBrain.Website.User:updateUser"
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
        nop (liftB . SetPasswd  target) mPass
      , nop (liftB . SetAdmin   target) mIsAd
      , nop (liftB . SetProfile target) mProf
      ]
    -- | Present the modified user:
    readUser target
  where
    permFail = respForbidden $ responseJSON'' "You must be logged in as admin or the user itself to perform updates."
    -- | Tries to apply a Maybe to a function in case of Just or performs a nop
    nop = maybe $ return ()

deleteUser :: UserId -> OBW Response
deleteUser target = do
  liftIO $ putStrLn "OpenBrain.Website.User:deleteUser"
  -- | Checking permissions:
  u <- liftB . GetUser =<< Session.chkSession
  plusm permFail $ do
    guard (isAdmin u || (userId u == target))
    -- | Deleting user:
    liftB $ do
      heir <- GetNobody
      DeleteUser target heir
    -- | Dropping session:
    when (userId u == target) Session.dropSession
    -- | Success messsage:
    respOk . responseJSON'' $ "Deleted user("++show target++")."
  where
    permFail = respForbidden $ responseJSON'' "You must be logged in as admin or the user itself to delete a user."

-- Security:
chkIsAdmin :: Bool -> OBW Bool
chkIsAdmin isA = plusm (return False) $ do
  uid <- Session.chkSession
  u   <- liftB $ GetUser uid
  guard $ isAdmin u
  return isA

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
getIsAdmin' = plusm (return Nothing) $ do
  isA  <- getIsAdmin
  isA' <- msum [chkIsAdmin isA, return False]
  return $ Just isA'

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
    someProfile = liftM (Just . Just) $ lookRead "profile"
    noProfile   = return Nothing
