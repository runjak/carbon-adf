{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module OpenBrain.Website.Action.Profile (serve) where

import Data.Aeson
import Data.Maybe
import Control.Monad
import Happstack.Server as Server
import qualified Data.ByteString.Lazy.UTF8 as LU

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
  prefix     <- msum [look "prefix",     return ""]
  foreName   <- msum [look "foreName",   return ""]
  middleName <- msum [look "middleName", return ""]
  familyName <- msum [look "familyName", return ""]
  suffix     <- msum [look "suffix",     return ""]
  let n = Name prefix foreName middleName familyName suffix
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

{-
  Expects parameter: locations - a list of JSON encoded OpenBrain.Data.Profile.Location
  Missing parameter will be understood as an empty list.
-}
setLocations :: OBW Response
setLocations = do
  ls <- msum [look "locations", return "[]"]
  let (ls' :: Maybe [Location]) = decode $ LU.fromString ls
  when (isNothing ls') $ rqError "Invalid JSON."
  p <- getProfile
  b <- gets backend
  liftIO $ B.setLocations b p $ fromJust ls'
  successMessage "Locations changed."

getSnippets :: String -> OBW [ProfileSnippet]
getSnippets parameter = do
  s <- msum [look parameter, return "[]"]
  let (s' :: Maybe [ProfileSnippet]) = decode $ LU.fromString s
  when (isNothing s') $ rqError "Invalid JSON."
  return $ fromJust s'

{-
  Expects parameter: websites - a list of JSON encoded OpenBrain.Data.Profile.ProfileSnippet
  Missing parameter will result in empty List
-}
setWebsites :: OBW Response
setWebsites = do
  websites  <- getSnippets "websites"
  p         <- getProfile
  b         <- gets backend
  liftIO $ B.setWebsites b p websites
  successMessage "Websites changed."

{-
  Expects parameter: emails - a list of JSON encoded OpenBrain.Data.Profile.ProfileSnippet
  Missing parameter will result in empty List
-}
setEmails :: OBW Response
setEmails = do
  emails  <- getSnippets "emails"
  p       <- getProfile
  b       <- gets backend
  liftIO $ B.setEmails b p emails
  successMessage "Emails changed."

{-
  Expects parameter: instantMessagers - a list of JSON encoded OpenBrain.Data.Profile.ProfileSnippet
  Missing parameter will result in empty List
-}
setInstantMessagers :: OBW Response
setInstantMessagers = do
  instantMessagers  <- getSnippets "instantMessagers"
  p                 <- getProfile
  b                 <- gets backend
  liftIO $ B.setInstantMessagers b p instantMessagers
  successMessage "InstantMessagers changed."

