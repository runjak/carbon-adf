{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Json where
{-|
  This module provides instances for [To,From]JSON
  so that modules in OpenBrain.Website.Json can handle data nicely.
|-}
import Control.Applicative as A
import Control.Monad
import Data.Aeson as Aeson
import Happstack.Server as S
import System.Time
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

jsonResponse :: (ToJSON j) => j -> Response
jsonResponse = setHeader "Content-Type" "application/json" . toResponse . encode

merge :: Value -> Value -> Value
merge (Object o1) (Object o2) = Object $ HashMap.union o1 o2
merge (Object o1) v = Object $ HashMap.insert "carry" v o1
merge o1 o2@(Object _) = merge o2 o1
merge x _ = x

merge' :: (ToJSON a, ToJSON b) => a -> b -> Value
merge' a = merge' $ toJSON a
