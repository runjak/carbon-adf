{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Datepicker where

import OpenBrain.Website.Template

datepicker :: IO HTML
datepicker = tmpl "Datepicker.html" emptyContext
