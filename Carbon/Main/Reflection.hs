{-# LANGUAGE TemplateHaskell #-}
module Carbon.Main.Reflection (version, date) where
{-|
  This module uses things found in Carbon.Main.CompileTime
  and prepares them for Carbon.Main.
|-}

import qualified Carbon.Main.CompileTime as CTime

version = tail . dropWhile (/=':') $ filter (not . flip elem " \t") $( CTime.version )

date = $( CTime.date )
