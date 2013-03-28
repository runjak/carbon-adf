{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common(
    module Common, module Crud, module Monad
  , contentNego, contentNego'
  , handleFail, jsonSuccess, jsonSuccess', jsonFail, jsonResponse
  , responseHTML
)where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Control.Monad
import Control.Monad.Trans
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as B
import Data.List (intercalate, isSuffixOf)
import Data.Maybe
import Data.String (IsString(..))
import Happstack.Server as S
import System.Time

import OpenBrain.Common        as Common
import OpenBrain.Data.Json     as Json
import OpenBrain.Website.Crud  as Crud 
import OpenBrain.Website.Monad as Monad

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

jsonSuccess :: String -> OBW Response
jsonSuccess = ok . jsonResponse . ActionStatus True

jsonSuccess' :: (ToJSON j) => String -> j -> OBW Response
jsonSuccess' msg = ok . jsonResponse . merge (ActionStatus True msg)

jsonFail :: String -> OBW Response
jsonFail = ok . jsonResponse . ActionStatus False

handleFail :: String -> OBW Response -> OBW Response
handleFail msg handle = msum [handle, jsonFail msg]

responseHTML :: Response -> Response
responseHTML = setHeader "Content-Type" "text/html"
