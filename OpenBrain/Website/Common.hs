{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common where
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
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types
import OpenBrain.Common
import OpenBrain.Website.Monad

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

instance ToMarkup CalendarTime where
  toMarkup t =  H.toHtml $
    show (ctHour t) ++ ":" ++ show (ctMin t) ++ " " ++ show (ctDay t) ++ "." ++ show (ctMonth t) ++ " " ++ show (ctYear t)

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

toHref :: String -> [String] -> H.AttributeValue
toHref target parameters = H.toValue $ target ++ "?" ++ intercalate "&" parameters

handleFail :: String -> OBW Response -> OBW Response
handleFail msg handle = msum [handle, ok (toResponse $ "FAIL: " ++ msg)]

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
pages :: Limit -> Offset -> Count -> [(String, Limit)]
pages limit offset count = do
  let page  = offset `div` limit
      cutoff = take 5 . drop 1
      mkPositive = map (\x->(x>0)?(x,0)) . (\(as,bs) -> as ++ [head bs]) . break (<=0)
      prevs = cutoff . mkPositive $ iterate (subtract limit) offset
      nexts = cutoff . takeWhile (< count) $ iterate (+ limit) offset
  zip (map show . drop 1 $ iterate (subtract 1) page) prevs
    ++ [("Current",page)]
    ++ zip (map show [(page + 1)..]) nexts

