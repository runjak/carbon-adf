module Carbon.Data.Logic.Diamond(
  DiamondResult(..)
, ResultType(..)
, Results(..)
, isEmpty
, implications
)where

import Control.Arrow (second)
import Data.List (nub)

import Carbon.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Parse

{-|
  Datatypes that define the results
|-}
data DiamondResult a = DiamondResult {
    inSet   :: [a]
  , udecSet :: [a]
  , outSet  :: [a]
  }

data ResultType = TwoValued
                | Stable
                | Grounded
                | Complete
                | Admissible
                | Preferred
                deriving (Show, Read, Eq, Enum, Bounded, Ord)

newtype Results a = Results [(ResultType, [DiamondResult a])]

isEmpty :: Results a -> Bool
isEmpty (Results r) = null r

-- | admissible >= complete >= (grounded, pref >= stable >= two-valued)
implications :: ResultType -> [ResultType]
implications Admissible = [Admissible]
implications Complete   = Complete  : implications Admissible
implications Grounded   = Grounded  : implications Complete
implications Preferred  = Preferred : implications Complete
implications Stable     = Stable    : implications Preferred
implications TwoValued  = TwoValued : implications Stable

{-| Instance declarations: |-}
instance Eq a => Eq (DiamondResult a) where
  d1 == d2 =
    let i  = inSet   d1 == inSet   d2
        u  = udecSet d1 == udecSet d2
        o  = outSet  d1 == outSet  d2
    in (i && u && o)

instance Eq a => Eq (Results a) where
  (Results r1) == (Results r2) = r1 == r2

instance Functor DiamondResult where
  fmap f d =
    let i  = map f $ inSet   d
        u  = map f $ udecSet d
        o  = map f $ outSet  d
    in DiamondResult i u o

instance Functor Results where
  fmap f (Results r) = Results $ map (second . map $ fmap f) r

instance Show a => Show (DiamondResult a) where
  show d =
    let i = map (\e -> "in("   ++ show e ++ ")") $ inSet   d
        u = map (\e -> "udec(" ++ show e ++ ")") $ udecSet d
        o = map (\e -> "out("  ++ show e ++ ")") $ outSet  d
    in unwords $ concat [i,u,o]

instance Show a => Show (Results a) where
  show (Results r) = unlines $ concatMap go r
    where
      go (t, drs)     = tell t ++ answers drs ++ end
      answers         = zipWith (\n d -> show n ++ ":\t" ++ show d) [1..]
      tell TwoValued  = "two-valued models:":end
      tell Stable     = "stable models:":end
      tell Grounded   = "grounded models:":end
      tell Complete   = "complete models:":end
      tell Admissible = "admissible model:":end
      tell Preferred  = "preferred models:":end
      end = ["=============================="]

instance StartState (DiamondResult a) where
  startState = DiamondResult [] [] []

instance StartState (Results a) where
  startState = Results []
