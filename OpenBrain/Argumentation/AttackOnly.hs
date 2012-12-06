{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module OpenBrain.Argumentation.AttackOnly where

import Control.Monad.Reader as Reader
import qualified Data.Set   as Set

import OpenBrain.Argumentation.Semantics as Semantics

data AttackContext = AC {
    attackgraph  :: Graph
  , tAttackGraph :: Graph  
  }

type Attack = Reader AttackContext

instance ArgumentationFramework Attack where
--  conflictFree :: [Argument] -> Attack Bool
  conflictFree args = do
    let aSet = Set.fromList args
    undefined
--  acceptable :: Argument -> [Argument] -> Attack Bool
  acceptable a args = undefined
--  acceptables :: [Argument] -> Attack [Argument]
  acceptables args = undefined
--  admissible :: [Argument] -> Attack Bool
  admissible args = undefined
--  admissibleSets :: Attack [[Argument]]
  admissibleSets = undefined
--  unattacked :: [Argument] -> Attack Bool
  unattacked args = undefined
--  unattackedSets :: Attack [[Argument]]
  unattackedSets = undefined
--  restriction :: [Argument] -> Attack ()
  restriction args = undefined
--  complete :: [Argument] -> Attack Bool
--  complete args = undefined
--  complete has a default method.
--  completeSets :: Attack [[Argument]]
  completeSets = undefined
--  grounded :: [Argument] -> Attack Bool
  grounded args = undefined
--  groundedSets :: Attack [[Argument]]
  groundedSets = undefined
--  stable :: [Argument] -> Attack Bool
  stable args = undefined
--  preferred :: [Argument] -> Attack Bool
  preferred args = undefined
--  preferredSets :: Attack [[Argument]]
  preferredSets = undefined

