module OpenBrain.Data.Logic.Instance(
  ACondition(..)
, Statement(..)
, Instance(..)
, instanceFromAcs
)where

import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic.Exp
import OpenBrain.Data.Logic.Parse
import OpenBrain.Data.Logic.Renameable
import OpenBrain.Data.Logic.NameContainer

data ACondition = AC String Exp    deriving Eq
data Statement  = Statement String deriving Eq

data Instance = Instance {
    conditions :: [ACondition]
  , statements :: [Statement]
  } deriving Eq

instanceFromAcs :: [ACondition] -> Instance
instanceFromAcs acs = Instance acs . map Statement $ names acs

-- | Instances:
instance NameContainer ACondition where
  names (AC n e) = nub $ n:names e

instance NameContainer Statement where
  names (Statement s) = [s]

instance NameContainer Instance where
  names (Instance cs ss) = nub $ names cs ++ names ss

instance Show ACondition where
  show (AC n e) = "ac(" ++ n ++ "," ++ show e ++ ")."

instance Show Statement where
  show (Statement s) = "statement(" ++ s ++ ")."

instance Show Instance where
  show (Instance cs ss) = unlines $ map show cs ++ map show ss

instance StartState Instance where
  startState = Instance [] []

instance Renameable ACondition where
  rename m (AC n e) =
    case Map.lookup n m of
      (Just n') -> AC n' $ rename m e
      Nothing   -> AC n  $ rename m e

instance Renameable Statement where
  rename m st@(Statement s) = maybe st Statement $ Map.lookup s m

instance Renameable Instance where
  rename m (Instance cs ss) =
    let cs' = rename m cs
        ss' = rename m ss
    in Instance cs' ss'
