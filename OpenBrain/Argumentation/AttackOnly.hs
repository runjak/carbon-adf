{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module OpenBrain.Argumentation.AttackOnly where

import Control.Monad.Reader as Reader
import Data.List ((\\))
import qualified Data.List  as List

import OpenBrain.Argumentation.Semantics as Semantics
import OpenBrain.Common

newtype AttackContext = A Graph

type Attack = Reader AttackContext

graph :: Attack Graph
graph = asks $ \(A g) -> g

{-|
  Reused default methods:
    complete, grounded, groundedSets, preferred, preferredSets
  , unattackedSets, admissibleSets, completeSets
|-}
instance ArgumentationFramework Attack where
--  arguments :: af [Argument]
  arguments = liftM vertices graph
--  conflictFree :: [Argument] -> Attack Bool
  conflictFree args = do
    targets <- liftM (flip next' args) graph
    return . null $ List.intersect args targets
--  acceptable :: Argument -> [Argument] -> Attack Bool
  acceptable a args = do
    attackers <- liftM (flip prev a) graph
    victims   <- liftM (flip next' args) graph
    return . null $ attackers \\ victims -- | All attackers of a are victims of args
--  acceptables :: [Argument] -> Attack [Argument]
  acceptables args = filterM (flip acceptable args) =<< liftM vertices graph
--  admissible :: [Argument] -> Attack Bool
  admissible args = do
    isCfree <- conflictFree args
    areAcceptables <- liftM and $ mapM (\a -> acceptable a args) args
    return $ isCfree && areAcceptables
--  unattacked :: [Argument] -> Attack Bool
  unattacked args = do
    attackers <- liftM (List.nub . concat) $
                  mapM (\a -> liftM (flip prev a) graph) args
    return . List.null $ attackers \\ args
--  stable :: [Argument] -> Attack Bool
  stable args = do
    isCf <- conflictFree args
    case isCf of
      False -> return False
      True  -> do
        victims <- liftM (flip next' args) graph
        others  <- liftM ((\\ args) . vertices) graph
        return $ victims == others

