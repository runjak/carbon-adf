{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}
import Data.Aeson
import Data.Maybe
import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server as S

import OpenBrain.Backend (Backend, UserBackend, KarmaBackend, SaltShaker)
import qualified OpenBrain.Backend as B
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash, hash)
import OpenBrain.Data.Salt (Salt, mkSalt)
import OpenBrain.Website.Action.Common (jToResponse, failMessage)
import OpenBrain.Website.Common
import OpenBrain.Website.Session

serve :: Backend -> ServerPartT IO Response
serve b = msum [
    dir "create"  $ create b
  , dir "login"   $ login  b
  , dir "logout"  $ logout b
  , dir "delete"  $ delete b
  ]

{-
  Expects get parameters: username, password
-}
create :: Backend -> ServerPartT IO Response
create b = do
  username <- look "username"
  salt <- liftIO mkSalt
  hash <- liftM (hash salt) $ look "password"
  mUserData <- liftIO $ B.register (B.userBackend b) username hash
  case mUserData of
    Nothing -> failMessage "Fail: Could not register user."
    (Just userdata) -> do
      mkSession b $ userid userdata
      ok . jToResponse $ object [
          "message" .= ("User created." :: String)
        , "success" .= True
        ]

{-
  Expects get parameters: username, password
-}
login :: Backend -> ServerPartT IO Response
login b = do
  username <- look "username"
  mUid <- liftIO $ B.hasUserWithName (B.userBackend b) username
  case mUid of
    Nothing -> failMessage "Fail: Login failed."
    (Just uid) -> do
      salt <- liftIO $ B.getSalt (B.saltShaker b) uid
      hash <- liftM (hash salt) $ look "password"
      mUserData <- liftIO $ B.login (B.userBackend b) username hash
      case mUserData of
        Nothing -> failMessage "Fail: Login failed."
        (Just userdata) -> do
          let uid = userid userdata
          mkSession b uid
          ok . jToResponse $ object [
              "message" .= ("Login complete" :: String)
            , "success" .= True
            , "userid"  .= uid
            , "karma"   .= (show $ karma userdata)
            , "isAdmin" .= isAdmin userdata
            ]

logout :: Backend -> ServerPartT IO Response
logout b = do
  dropSession b
  ok . jToResponse $ object [
      "message" .= ("Logout complete" :: String)
    , "success" .= True
    ]

{- Expects get parameters: username -}
delete :: Backend -> ServerPartT IO Response
delete b = do
  deletename <- liftM read $ look "username"
  muid <- chkSession b
  case muid of
    Nothing -> failMessage "Invalid session"
    (Just uid) -> do
      mUser <- liftIO $ B.getUser (B.userBackend b) uid
      mDeleteId <- liftIO $ B.hasUserWithName (B.userBackend b) deletename
      let mUserDeleteId = liftM2 (,) mUser mDeleteId
      case mUserDeleteId of
        Nothing -> failMessage "Data for deletion not found"
        Just (userData, deleteId) -> do
          deleteKarma <- liftIO . B.karmaDeleteUser $ B.karmaBackend b
          let allowed = or [userid userData == deleteId, isAdmin userData, karma userData >= deleteKarma]
          case allowed of
            False -> failMessage "Not allowed to delete requested user."
            True -> do
              success <- liftIO $ B.delete (B.userBackend b) deleteId
              dropSession b
              ok . jToResponse $ object [
                    "message" .= ("Deleted requested user."::String)
                  , "success" .= success
                ]
