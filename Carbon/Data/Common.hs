{-# LANGUAGE MultiParamTypeClasses #-}
module Carbon.Data.Common where

mergeStr :: String -> String -> String
mergeStr s "" = s
mergeStr _  s = s

-- Notes that into 'a' a 'b' can be inserted by the means of (<+>).
class Insertable a b where
  (<+) :: a -> b -> a
  infixr 6 <+
