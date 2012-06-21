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
import Data.Maybe
import Happstack.Server as S

import OpenBrain.Config
import OpenBrain.Backend
import OpenBrain.Data.User

cookie_actionKey  = "actionKey" :: String
cookie_userid     = "userid"    :: String

class UserSession u where
  mkSession   :: u -> UserId -> ServerPart ()   -- Initialize a session
  chkSession  :: u -> ServerPart (Maybe UserId) -- Check if the Session is valid
  chkAction   :: u -> ServerPart (Maybe UserId) -- Like chkSession but changes the ActionKey
  dropSession :: u -> ServerPart ()

instance UserSession SessionManagement where
  mkSession sm userid = do
    key <- liftIO $ startSession sm userid
    addCookies $ map ((,) Session) [mkCookie cookie_userid $ show userid, mkCookie cookie_actionKey key]
  chkSession sm = do
    key    <- lookCookieValue cookie_actionKey
    userid <- liftM read $ lookCookieValue cookie_userid
    valid  <- liftIO $ validate sm userid key
    guard valid
    return $ Just userid
    `mplus` (return Nothing)
  chkAction sm = do
    key    <- lookCookieValue cookie_actionKey
    userid <- liftM read $ lookCookieValue cookie_userid
    mkey   <- liftIO $ perform sm userid key
    guard $ isJust mkey
    addCookie Session (mkCookie cookie_actionKey $ fromJust mkey) >> return (Just userid)
    `mplus` (return Nothing)
  dropSession sm = do
    key    <- lookCookieValue cookie_actionKey
    userid <- liftM read $ lookCookieValue cookie_userid
    liftIO $ stopSession sm userid key
    sequence_ $ map expireCookie [cookie_actionKey, cookie_userid]

instance UserSession Backend where
  mkSession   = mkSession   . sessionManagement
  chkSession  = chkSession  . sessionManagement
  chkAction   = chkAction   . sessionManagement
  dropSession = dropSession . sessionManagement

