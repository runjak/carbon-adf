{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common(
    module Common, module Monad, module Template
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
import Text.Hastache
import Text.Hastache.Context

import OpenBrain.Common           as Common
import OpenBrain.Website.Monad    as Monad
import OpenBrain.Website.Template as Template

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
pages :: Limit -> Offset -> Count -> LinkBase -> OBW HTML
pages l o c lBase = go $ pages' l o c
  where
    go :: [(String, Limit)] -> OBW HTML
    go ps = do
      let context "HasPages" = MuBool . not $ null ps
          context "Pages"    = MuList $ map pageContext ps
      liftIO $ tmpl "Pages.html" context

    pageContext (n, l) "PageLink" = MuVariable $ lBase ++ "&limit=" ++ show l
    pageContext (n, l) "PageTitle" = MuVariable n
    
    pages' :: Limit -> Offset -> Count -> [(String, Limit)]
    pages' 0 _ _ = []
    pages' _ _ 0 = []
    pages' _ 0 _ = []
    pages' limit offset count = do
      let page       = offset `div` limit
          cutoff     = take 5 . drop 1
          mkPositive = map (max 0) . (\(as,bs) -> as ++ [head bs]) . break (<=0)
          prevs      = cutoff . mkPositive $ iterate (subtract limit) offset
          nexts      = cutoff . takeWhile (< count) $ iterate (+ limit) offset
      zip (map show . drop 1 $ iterate (subtract 1) page) prevs
        ++ [("Current",page)]
        ++ zip (map show [(page + 1)..]) nexts
