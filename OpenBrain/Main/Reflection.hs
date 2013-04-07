{-# LANGUAGE TemplateHaskell #-}
module OpenBrain.Main.Reflection (version, info) where
{-|
  This module uses things found in OpenBrain.Main.CompileTime
  and prepares them for OpenBrain.Main.
|-}

import qualified OpenBrain.Main.CompileTime as CTime

version = tail . snd . break (==':') $ filter (not . flip elem " \t") $( CTime.version )

info = unlines $
  [ "Date of compilation:\t"
  ++ $(CTime.date)] ++
  [ "------------------"
  , "Commits -> Author:"] ++
    lines $( CTime.authors ) ++
  [ "-----------------------"
  , "Total lines of Haskell:\t\t"
  ++ linesOfHs
  , "Total lines of JavaScript:\t"
  ++ linesOfJs]

linesOfHs = lHelp $( CTime.linesOfHs )
linesOfJs = lHelp $( CTime.linesOfJs )
lHelp = head . words . last . lines
