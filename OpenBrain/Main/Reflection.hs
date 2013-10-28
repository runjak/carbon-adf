{-# LANGUAGE TemplateHaskell #-}
module OpenBrain.Main.Reflection (version, date) where
{-|
  This module uses things found in OpenBrain.Main.CompileTime
  and prepares them for OpenBrain.Main.
|-}

import qualified OpenBrain.Main.CompileTime as CTime

version = tail . snd . break (==':') $ filter (not . flip elem " \t") $( CTime.version )

date = $( CTime.date )
