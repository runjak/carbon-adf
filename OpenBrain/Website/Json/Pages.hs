module OpenBrain.Website.Json.Pages(serve)where

import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Parameters as Parameters

{-|
  Produces a set of Offsets for a given type of list and
  a given limit.
  /pages/_?limit=_
|-}
serve :: OBW Response
serve = path . serve' =<< Parameters.getLimit
  where
    serve' :: Limit -> String -> OBW Response
    serve' l "user" = do
      c <- liftOBB GetUserCount
      jsonSuccess' ("All pages for users with limit:\t" ++ show l) $ pages l c
    serve' l "information" = do
      c <- liftOBB GetInformationCount
      jsonSuccess' ("All pages for information wiht limit:\t" ++ show l) $ pages l c
    serve' _ t = jsonFail $ "Could not find list type:\t" ++ t

{-
  Calculates a set of Offsets for a given combination of Limit and Count.
-}
pages :: Limit -> Count -> [Offset]
pages l c
  | l == 0    = [0]
  | c <= l    = [0]
  | otherwise = map (*l) [0..(c `div` l)]
