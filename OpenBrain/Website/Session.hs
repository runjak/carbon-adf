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
import Happstack.Server as S

import OpenBrain.Config
import OpenBrain.Backend
import OpenBrain.Data.User

cookie_actionKey  = "actionKey" :: String
cookie_userid     = "userid"    :: String

class UserSession u where
  mkSession   :: u -> UserId -> ServerPartT IO ()
  chkSession  :: u -> ServerPartT IO (Maybe UserId)
  dropSession :: u -> ServerPartT IO ()

instance UserSession SessionManagement where
  mkSession sm userid = do
    key <- liftIO $ startSession sm userid
    let cookies = [mkCookie cookie_userid $ show userid, mkCookie cookie_actionKey key]
    sequence_ $ map (addCookie Session) cookies
  chkSession sm = do
    key <- lookCookieValue cookie_actionKey
    userid <- liftM read $ lookCookieValue cookie_userid
    mKey <- liftIO $ validate sm userid key
    case mKey of
      Nothing -> return Nothing
      (Just k) -> addCookie Session (mkCookie cookie_actionKey k) >> return (Just userid)
  dropSession sm = do
    key <- lookCookieValue cookie_actionKey
    userid <- liftM read $ lookCookieValue cookie_userid
    liftIO $ stopSession sm userid key
    sequence_ $ map expireCookie [cookie_actionKey, cookie_userid]

instance UserSession Backend where
  mkSession = mkSession . sessionManagement
  chkSession = chkSession . sessionManagement
  dropSession = dropSession . sessionManagement

