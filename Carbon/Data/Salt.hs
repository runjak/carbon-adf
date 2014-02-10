{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Carbon.Data.Salt (Salt, toSalt, fromSalt, mkSalt) where

import Control.Monad
import System.Random

newtype Salt = Salt String deriving (Eq, Ord, Show, Read)

toSalt :: String -> Salt
toSalt = Salt

fromSalt :: Salt -> String
fromSalt (Salt s) = s

mkSalt :: IO Salt
mkSalt = do
  (r:rs) <- liftM randoms newStdGen
  let l   = 10 + (r `mod` 11)
      key = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
  return $ Salt key

