{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Website.Session (UserSession(..)) where
{-
  Session management for clients.
  Each client will get an actionkey set on login.
  Actionkeys are saved in a cookie clientside
  and in the Backend serverside.
  Whenever a logged in client performs an action,
  it will send it's actionkey and will get a new one in exchange.
-}

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Happstack.Server as S

import OpenBrain.Config
import OpenBrain.Backend
import OpenBrain.Backend.Plus
import OpenBrain.Data.User

cookieActionKey  = "actionKey" :: String
cookieUserId     = "userid"    :: String

class UserSession u where
  mkSession   :: u -> UserId -> ServerPart ()   -- Initialize a session
  chkSession  :: u -> ServerPart (Maybe UserId) -- Check if the Session is valid
  chkAction   :: u -> ServerPart (Maybe UserId) -- Like chkSession but changes the ActionKey
  dropSession :: u -> ServerPart ()

instance UserSession CSessionManagement where
  mkSession sm userid = do
    key <- liftIO $ startSession sm userid
    addCookies $ map ((,) Session) [mkCookie cookieUserId $ show userid, mkCookie cookieActionKey key]
  chkSession sm = do
    key    <- lookCookieValue cookieActionKey
    userid <- liftM read $ lookCookieValue cookieUserId
    valid  <- liftIO $ validate sm userid key
    guard valid
    return $ Just userid
    `mplus` return Nothing
  chkAction sm = do
    key    <- lookCookieValue cookieActionKey
    userid <- liftM read $ lookCookieValue cookieUserId
    mkey   <- liftIO $ runMaybeT $ perform sm userid key
    guard $ isJust mkey
    addCookie Session (mkCookie cookieActionKey $ fromJust mkey) >> return (Just userid)
    `mplus` return Nothing
  dropSession sm = do
    key <- lookCookieValue cookieActionKey
    (userid :: UserId) <- liftM read $ lookCookieValue cookieUserId
    liftIO $ stopSession sm userid key
    mapM_ expireCookie [cookieActionKey, cookieUserId]

instance UserSession CBackend where
  mkSession   = mkSession   . sessionManagement
  chkSession  = chkSession  . sessionManagement
  chkAction   = chkAction   . sessionManagement
  dropSession = dropSession . sessionManagement

