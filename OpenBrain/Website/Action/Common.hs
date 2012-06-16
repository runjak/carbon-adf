{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module OpenBrain.Website.Action.Common (jToResponse, failMessage, successMessage) where
{-
  Things to be used across modules of OpenBrain.Website.Action.*
-}

import Data.Aeson
import Happstack.Server

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
