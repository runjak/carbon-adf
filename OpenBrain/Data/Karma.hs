module OpenBrain.Data.Karma (
    Karma, newKarma           -- Type and creation of it
  , toKarma, fromKarma        -- simple conversion from/to Int
  , hasKarma, satisfiesRatio  -- logic build upon it
) where
{-
  Karma is a system to map user experience and cooperation
  to a gain of privileges in the application.
  This aims to decentralise administration.
  Karma is basically an Int in the Set of N0.
-}

import Control.Arrow ((***))

newtype Karma = Karma Int

newKarma :: Karma
newKarma = toKarma 0

toKarma :: Int -> Karma
toKarma = Karma . max 0

fromKarma :: Karma -> Int
fromKarma (Karma k) = k

hasKarma :: (Num level, Integral level) => level -> Karma -> Bool
hasKarma level k = let x = fromInteger (toInteger level) in k >= x

instance Eq Karma where
  (Karma a) == (Karma b) = a == b

instance Ord Karma where
  compare (Karma a) (Karma b) = compare a b

instance Show Karma where
  show = show . fromKarma

instance Num Karma where
  (Karma a) + (Karma b) = Karma (a + b)
  (Karma a) * (Karma b) = Karma (a * b)
  (Karma a) - (Karma b)
    | b > a = Karma 0
    | otherwise = Karma (a - b)
  negate (Karma a) = Karma 0
  abs = id
  signum (Karma a)
    | a == 0 = Karma 0
    | otherwise = Karma a
  fromInteger = Karma . fromInteger

instance Bounded Karma where
  minBound = Karma 0
  maxBound = Karma maxBound

instance Real Karma where
  toRational = toRational . fromKarma

instance Enum Karma where
  succ = toKarma . succ . fromKarma
  pred = toKarma . pred . fromKarma
  toEnum = toKarma
  fromEnum = fromKarma

instance Integral Karma where
  quot (Karma a) (Karma b) = toKarma $ quot a b
  rem (Karma a) (Karma b) = toKarma $ rem a b
  div (Karma a) (Karma b) = toKarma $ div a b
  mod (Karma a) (Karma b) = toKarma $ mod a b
  quotRem (Karma a) (Karma b) = (toKarma *** toKarma) $ quotRem a b
  divMod (Karma a) (Karma b) = (toKarma *** toKarma) $ divMod a b
  toInteger = toInteger . fromKarma

{- Describes the highest current value of Karma in the system. -}
type Highest = Karma
{-
  Is a function that computes the required amount of Karma
  to satisfy a ratio in realation to the highest current value.
-}
satisfiesRatio :: Rational -> Highest -> Karma
satisfiesRatio rat h =
  let spaceToTop = toKarma . ceiling . (rat *) $ toRational h
  in h - spaceToTop
