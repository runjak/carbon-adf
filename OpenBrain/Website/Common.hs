{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.String (IsString(..))
import Happstack.Server as S
import System.Time
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

instance ToMarkup CalendarTime where
  toMarkup t =  H.toHtml $
    show (ctHour t) ++ ":" ++ show (ctMin t) ++ " " ++ show (ctDay t) ++ "." ++ show (ctMonth t) ++ " " ++ show (ctYear t)

contentNego :: String -> OBW Response
contentNego base = dir base $ do
  contentType <- liftM (maybe "" id) $ lift $ getHeaderM "Accept"
  let suffix = suffixTable contentType
  let target = base ++ suffix
  lift $ ok $ toResponse $ "303 redirect to " ++ target
--  lift $ seeOther target $ toResponse $ "303 redirect to " ++ target
  where
    suffixTable :: ByteString -> String
    suffixTable contentType
      | "application/json" `isInfixOf` contentType = ".json"
      | "text/html" `isInfixOf` contentType = ".html"
      | otherwise = ".html"

toHref :: String -> [String] -> H.AttributeValue
toHref target parameters = H.toValue $ target ++ "?" ++ intercalate "&" parameters

handleFail :: String -> OBW Response -> OBW Response
handleFail msg handle = msum [handle, ok (toResponse msg)]
