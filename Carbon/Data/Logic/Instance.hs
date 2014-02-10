{-#LANGUAGE FlexibleInstances#-}
module Carbon.Data.Logic.Instance(
  ACondition(..)
, Statement(..)
, Instance(..)
, instanceFromAcs
)where

import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import Data.Set (Set)
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import Carbon.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Exp
import Carbon.Data.Logic.Parse
import Carbon.Data.Logic.Renameable
import Carbon.Data.Logic.NameContainer

data ACondition a = AC {aHead :: a, aCondition :: Exp a}
data Statement  a = Statement a

data Instance a = Instance {
    conditions :: [ACondition a]
  , statements :: [Statement  a]
  }

instanceFromAcs :: [ACondition String] -> Instance String
instanceFromAcs acs = Instance acs . map Statement . Set.toList $ names acs

-- | Instances:
instance Eq a => Eq (ACondition a) where
  a1 == a2 =
    let eqHeads = aHead a1      == aHead a2
        eqConds = aCondition a1 == aCondition a2
    in eqHeads && eqConds

instance Eq a => Eq (Statement a) where
  (Statement x) == (Statement y) = x == y

instance Eq a => Eq (Instance a) where
  i1 == i2 =
    let eqConds = conditions i1 == conditions i2
        eqStats = statements i1 == statements i2
    in eqConds && eqStats

instance Functor ACondition where
  fmap f (AC h c) = AC (f h) $ fmap f c

instance Functor Statement where
  fmap f (Statement s) = Statement $ f s

instance Functor Instance where
  fmap f (Instance cs ss) = Instance (map (fmap f) cs) (map (fmap f) ss)

instance NameContainer (ACondition String) where
  names ac = Set.insert (aHead ac) . names $ aCondition ac

instance NameContainer (Statement String) where
  names (Statement s) = Set.fromList [s]

instance NameContainer (Instance String) where
  names i = names (conditions i) `Set.union` names (statements i)

instance Show (ACondition String) where
  show (AC n e) = "ac(" ++ n ++ "," ++ show e ++ ")."

instance Show (Statement String) where
  show (Statement s) = "statement(" ++ s ++ ")."

instance Show (Instance String) where
  show (Instance cs ss) = unlines $ map show cs ++ map show ss

instance StartState (Instance String) where
  startState = Instance [] []
