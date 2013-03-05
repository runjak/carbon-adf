{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common(
    module Common, module Monad
  , contentNego, contentNego'
  , handleFail, doFail, handleSuccess
  , LinkBase
  , pages
)where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as B
import Data.List (intercalate, isSuffixOf)
import Data.Maybe
import Data.String (IsString(..))
import Happstack.Server as S
import System.Time

import OpenBrain.Common           as Common
import OpenBrain.Website.Monad    as Monad

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

contentNego :: String -> OBW Response
contentNego base = do
  guard $ needsContentNego base
  contentType <- liftM (fromMaybe "") $ lift $ getHeaderM "Accept"
  let suffix = suffixTable contentType
  let target = base ++ suffix
  lift $ seeOther target $ toResponse $ "303 redirect to " ++ target
  where
    needsContentNego :: String -> Bool
    needsContentNego s
      | ".html" `isSuffixOf` s = False
      | ".json" `isSuffixOf` s = False
      | otherwise = True
    suffixTable :: ByteString -> String
    suffixTable contentType
      | "application/json" `isInfixOf` contentType = ".json"
      | "text/html" `isInfixOf` contentType = ".html"
      | otherwise = ".html"

{- In difference to contentNego this function also matches the dir. -}
contentNego' :: String -> OBW Response
contentNego' base = dir base $ contentNego base

handleFail :: String -> OBW Response -> OBW Response
handleFail msg handle = msum [handle, doFail msg]

doFail :: String -> OBW Response
doFail = ok . toResponse . ("FAIL: " ++)

handleSuccess :: String -> OBW Response
handleSuccess = ok . toResponse

{-
  Calculates pages as names and offsets for a given combination
  of Limit, Offset and Count.
  Up to 5 pages precede the "Current" page,
  up to 5 follow it.
  All pages are generated so that all Items can be listed,
  but Limit + Offset may be bigger than Count.
-}
type LinkBase = String -- Will get ++"&limit=l"
pages :: Limit -> Count -> [Offset]
pages l c
  | c <= l    = [0]
  | otherwise = map (*l) [0..(c `div` l)]
