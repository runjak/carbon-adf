{-# LANGUAGE ExistentialQuantification #-}
module Backend (
    Backend(..)
  , loadBackend
  , ProxyBackend
  , mkProxy
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import Config

class Backend b where
  foo :: b -> IO ()

loadBackend :: Config -> IO (Maybe ProxyBackend)
loadBackend = undefined

-- | To enable lists and stuff .)
data ProxyBackend = forall b . Backend b => PB b
instance Backend ProxyBackend where
  foo (PB b) = foo b

mkProxy :: (Backend b) => b -> ProxyBackend
mkProxy = PB
