{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Login(
  login, logout
)where

import Data.Maybe

import Carbon.Website.Common
import qualified Carbon.Website.Session as Session
import qualified Carbon.Website.User    as User

login :: OBW Response
login = plusm params $ do
  uname <- User.getUsername
  mHash <- User.getPassword'
  plusm failed $ do
    muid <- liftB $ HasUser uname
    guard $ isJust muid && isJust mHash
    let uid = fromJust muid
    mskey <- liftB $ Login uid (fromJust mHash)
    guard $ isJust mskey
    Session.mkSession uid $ fromJust mskey
    respOk $ responseJSON'' "Login successful."
  where
    params = respBadRequest $ responseJSON'' "Expected parameters are:\t username, password"
    failed = respForbidden  $ responseJSON'' "Wrong username or password."

logout :: OBW Response
logout = plusm notLoggedIn $ doLogout >> Session.dropSession >> success
  where
    notLoggedIn = respBadRequest $ responseJSON'' "Not logged in."
    doLogout    = liftB . Logout =<< Session.chkSession
    success     = respOk $ responseJSON'' "Logout succesful."
