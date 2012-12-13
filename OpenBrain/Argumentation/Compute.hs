{-# Language ExistentialQuantification, Rank2Types #-}
module OpenBrain.Argumentation.Compute where
{-|
  This module helps with building an AF from a list of Relations.
|-}

import Data.List(nub)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import OpenBrain.Argumentation.Semantics
import OpenBrain.Backend.Types
import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Data.Relation as R

type SimpleRel = (InformationId, RelationType, InformationId)

simplify :: R.Relation -> SimpleRel
simplify r =
  let src = source r
      rel = relation r
      tar = target r
  in (src, rel, tar)

{-|
  The returned mapping functions only work on defined values
  for InformationId that are found in the given relations
  and for values of Argument as produced by the lookup functions.
|-}
mkMapping :: [SimpleRel] -> (InformationId -> Argument, Argument -> InformationId)
mkMapping rels =
  let iids   = nub $ concatMap (\(r1, _, r2) -> [r1, r2]) rels
      inMap  = Map.fromList $ zip iids [0..]
      niMap  = Map.fromList $ zip [0..] iids
      getArg = fromJust . flip Map.lookup inMap
      getIid = fromJust . flip Map.lookup niMap
  in (getArg, getIid)

class Monad m => ComputeContext m where
  {-|
    The BuildContext allows us to use a list of SimpleRel
    to extract a list of Argument in m as a list of InformationId.
  |-}
  compute' :: [SimpleRel] -> m [[Argument]] -> [[InformationId]]

compute :: ComputeContext m => [R.Relation] -> m [[Argument]] -> [[InformationId]]
compute = compute' . map simplify
