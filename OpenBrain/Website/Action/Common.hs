{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module OpenBrain.Website.Action.Common where
{-
  Things to be used across modules of OpenBrain.Website.Action.*
-}

import Control.Monad
import Data.Aeson
import Happstack.Server

import OpenBrain.Website.Monad

jToResponse :: (ToJSON j) => j -> Response
jToResponse = toResponse .  encode

failMessage :: (FilterMonad Response m) => String -> m Response
failMessage s = badRequest . jToResponse $ object [
    "message" .= s
  , "success" .= False  
  ]

successMessage :: (FilterMonad Response m) => String -> m Response
successMessage s = ok . jToResponse $ object [
    "message" .= s
  , "success" .= True
  ]

handleFail :: String -> OBW Response -> OBW Response
handleFail msg handle = msum [handle, failMessage msg]
