{-# LANGUAGE FlexibleInstances #-}
module OpenBrain.Data.Logic.Exp(
  Exp(..)
, VarContainer(..)
, and'
, or'
, removeVar
) where

import Control.Monad
import Data.Set (Set)
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic.Renameable
import OpenBrain.Data.Logic.NameContainer

data Exp  a = Var   a
            | And   (Exp a) (Exp a)
            | Or    (Exp a) (Exp a)
            | Neg   (Exp a)
            | Const Bool
            deriving Eq

class VarContainer v where
  vars :: (Ord a) => v a -> Set a

-- | Instances:
instance Functor Exp where
  fmap f (Var x)     = Var $ f x
  fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
  fmap f (Or  e1 e2) = Or  (fmap f e1) (fmap f e2)
  fmap f (Neg e)     = Neg $ fmap f e
  fmap f (Const b)   = Const b

instance NameContainer (Exp String) where
  names = vars

instance Show (Exp String) where
  show (Var s)   = s
  show (And x y) = "and(" ++ show x ++ "," ++ show y ++ ")"
  show (Or  x y) =  "or(" ++ show x ++ "," ++ show y ++ ")"
  show (Neg x)   = "neg(" ++ show x ++ ")"
  show (Const t) =   "c(" ++ (t ? ("v","f")) ++ ")"

instance VarContainer Exp where
  vars (Var a)     = Set.fromList [a]
  vars (And e1 e2) = vars e1 `Set.union` vars e2
  vars (Or  e1 e2) = vars e1 `Set.union` vars e2
  vars (Neg e)     = vars e
  vars (Const _)   = Set.empty

{-|
  Mechanisms to enable easier work with Exp:
|-}
and' = foldr1 And
or'  = foldr1 Or

removeVar :: Eq a => a-> Exp a -> Exp a
removeVar s = Maybe.fromMaybe (Const True) . removeVar' s
  where
    removeVar' :: Eq a => a -> Exp a -> Maybe (Exp a)
    removeVar' s v@(Var s')
      | s == s'   = Nothing
      | otherwise = Just v
    removeVar' s (And e1 e2) =
      let e1' = removeVar' s e1
          e2' = removeVar' s e2
      in case (e1', e2') of
        (Just a,   Just b) -> Just $ And a b
        (Just a,  Nothing) -> e1'
        (Nothing,  Just b) -> e2'
        (Nothing, Nothing) -> Nothing
    removeVar' s (Or e1 e2) =
      let e1' = removeVar' s e1
          e2' = removeVar' s e2
      in case (e1', e2') of
        (Just a,   Just b) -> Just $ Or a b
        (Just a,  Nothing) -> e1'
        (Nothing,  Just b) -> e2'
        (Nothing, Nothing) -> Nothing
    removeVar' s (Neg e) =
      let e' = removeVar' s e
      in case e' of
        (Just e) -> Just $ Neg e
        Nothing  -> Nothing
    removeVar' s c@(Const _) = Just c

