{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.Salt (Salt, toSalt, fromSalt, mkSalt) where

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
  let l = saltLength r
  return . Salt . take l $ map saltChar rs

saltLength :: Int -> Int
saltLength x = 10 + x `mod` 11

saltChar :: Int -> Char
saltChar = toEnum . (+) (fromEnum ' ') . (flip mod $ fromEnum '~' - fromEnum ' ')
