{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.Profile (serve) where

import Data.Aeson
import Control.Monad

import Happstack.Server as Server

import OpenBrain.Common
import OpenBrain.Data.Profile
import OpenBrain.Data.User
import OpenBrain.Website.Action.Common hiding (jToResponse)
import OpenBrain.Website.Monad as M
import qualified OpenBrain.Backend as B
import qualified OpenBrain.Website.Session.Plus as Session

serve :: OBW Response
serve = msum [
    dir "setAccessRule"       setAccessRule
  , dir "setName"             setName
  , dir "setAvatar"           setAvatar
  , dir "setLocations"        setLocations
  , dir "setWebsites"         setWebsites
  , dir "setEmails"           setEmails
  , dir "setInstantMessagers" setInstantMessagers
  ]

getProfile :: OBW Profile
getProfile = do
  b <- gets backend
  liftIO . (B.getProfile b) =<< Session.chkAction

{-
  Expects parameter: accessRule
  Fails on missing parameter.
-}
setAccessRule :: OBW Response
setAccessRule = do
  accessRule  <- lookRead "accessRule"
  profile     <- getProfile
  b           <- gets backend
  liftIO $ B.setAccessRule b profile accessRule
  successMessage "AccessRule changed."

instance FromReqURI AccessRule where
  fromReqURI s = case reads s of
    [(ar, "")]  -> Just ar
    _           -> Nothing

{-
  Expects parameters: prefix, foreName, middleName, familyName, suffix
  Missing parameters will result in empty fields.
-}
setName :: OBW Response
setName = do
  _prefix     <- msum [look "prefix",     return ""]
  _foreName   <- msum [look "foreName",   return ""]
  _middleName <- msum [look "middleName", return ""]
  _familyName <- msum [look "familyName", return ""]
  _suffix     <- msum [look "suffix",     return ""]
  let n = Name _prefix _foreName _middleName _familyName _suffix
  let n' = (n == emptyName) ? (Nothing, Just n)
  p <- getProfile
  b <- gets backend
  liftIO $ B.setName b p n'
  successMessage "Name changed."

{-
  Expects parameter: avatar
  Missing parameter will result in empty field.
-}
setAvatar :: OBW Response
setAvatar = do
  avatar <- msum [liftM Just $ look "avatar", return Nothing]
  p <- getProfile
  b <- gets backend
  liftIO $ B.setAvatar b p avatar
  successMessage "Avatar changed."

setLocations :: OBW Response
setLocations = undefined

setWebsites :: OBW Response
setWebsites = undefined

setEmails :: OBW Response
setEmails = undefined

setInstantMessagers :: OBW Response
setInstantMessagers = undefined

{-
  setLocations        :: (ProfileIdentifier pi) => p -> pi -> [Location] -> IO ()
  setWebsites         :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
  setEmails           :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
  setInstantMessagers :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
-}
