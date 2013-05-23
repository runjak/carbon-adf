{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common(
  module Id
, module Crud
, module Monad
, responseHTML, responseJSON, responseJSON'
, countAndPageBy, sanitize
)where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Data.Aeson (ToJSON)
import Data.String (IsString(..))
import Happstack.Server as S
import Text.Regex       as T
import qualified Data.Aeson as Aeson

import OpenBrain.Data.Id       as Id
import OpenBrain.Website.Crud  as Crud 
import OpenBrain.Website.Monad as Monad

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

responseHTML :: Response -> Response
responseHTML = setHeader "Content-Type" "text/html"

responseJSON :: Response -> Response
responseJSON = setHeader "Content-Type" "application/json"

responseJSON' :: ToJSON x => x -> Response 
responseJSON' = responseJSON . toResponse . Aeson.encode

countAndPageBy :: BackendDSL Count -> (Limit -> Offset -> BackendDSL Response) -> OBW Response
countAndPageBy c p = plusm count $ do
  (l, o) <- liftM2 (,) (lookRead "limit") $ lookRead "offset"
  respOk =<< liftB (p l o)
  where
    count = respOk . responseJSON' =<< liftB c

sanitize :: String -> String
sanitize = foldl1 (.) [
    \x -> subRegex (mkRegex "<") x "&lt;"
  , \x -> subRegex (mkRegex ">") x "&gt;"
  ]
