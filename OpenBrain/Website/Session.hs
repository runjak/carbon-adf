module OpenBrain.Website.Session where
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

import OpenBrain.Backend
import OpenBrain.Config
import OpenBrain.Website.Common

cookieActionKey = "actionKey"
cookieUserId    = "userid"

{-|
  Initializes a session for a given UserId
  and sets a cookie for the client.
|-}
mkSession :: UserId -> OBW ()
mkSession uid = do
  key <- liftOBB $ StartSession uid
  addCookies $ map ((,) Session) [mkCookie cookieUserId $ show uid, mkCookie cookieActionKey key]

{-|
  mzero on invalid session.
|-}
chkSession :: OBW UserId
chkSession = do
  key <- lookCookieValue cookieActionKey
  uid <- liftM read $ lookCookieValue cookieUserId
  guard =<< liftOBB (Validate uid key)
  return uid

chkSession' :: (UserId -> OBW Response) -> OBW Response
chkSession' f = handleFail "Login required" $ f =<< chkSession

dropSession :: OBW ()
dropSession = do
  key <- lookCookieValue cookieActionKey
  uid <- liftM read $ lookCookieValue cookieUserId
  liftOBB $ StopSession uid key
  mapM_ expireCookie [cookieActionKey, cookieUserId]

