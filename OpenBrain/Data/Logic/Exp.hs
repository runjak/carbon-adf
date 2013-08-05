module OpenBrain.Data.Logic.Exp(
  Exp(..)
, and'
, or'
, idToExp
) where

import Control.Monad
import Data.List (nub)
import qualified Data.Map as Map

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic.Renameable
import OpenBrain.Data.Logic.NameContainer

data Exp = Var   String
         | And   Exp Exp
         | Or    Exp Exp
         | Neg   Exp
         | Const Bool
         deriving Eq

-- | Instances:
instance NameContainer Exp where
  names = nub . names'
    where
      names' (Var   s) = [s]
      names' (And a b) = names' a ++ names' b
      names' (Or  a b) = names' a ++ names' b
      names' (Neg   a) = names' a
      names' (Const _) = []

instance Show Exp where
  show (Var s)   = s
  show (And x y) = "and(" ++ show x ++ "," ++ show y ++ ")"
  show (Or  x y) =  "or(" ++ show x ++ "," ++ show y ++ ")"
  show (Neg x)   = "neg(" ++ show x ++ ")"
  show (Const t) =   "c(" ++ (t ? ("v","f")) ++ ")"

instance Renameable Exp where
  rename m (And e f)   = And (rename m e) (rename m f)
  rename m (Or e f)    = Or  (rename m e) (rename m f)
  rename m (Neg e)     = Neg $ rename m e
  rename m c@(Const _) = c
  rename m v@(Var n)   = maybe v Var $ Map.lookup n m

{-|
  Mechanisms to enable easier work with Exp:
|-}
and' = foldr1 And
or'  = foldr1 Or

idToExp :: IdType i => i -> Exp
idToExp = Var . show . unwrap . toId
