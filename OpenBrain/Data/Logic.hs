module OpenBrain.Data.Logic(
  Exp(..), and', or'
, ACondition(..)
, parseExp, parseAc, parseAcs
, parseHelper, parseHelper'
, idToExp, renameVars, renameAc
)where


import Control.Monad
import Data.Map (Map)
import Happstack.Server (FromReqURI(..))
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Map as Map

import OpenBrain.Data.Id

data Exp = Var String
         | And Exp Exp
         | Or  Exp Exp
         | Neg Exp
         deriving Eq

and' = foldr1 And
or'  = foldr1 Or

instance Show Exp where
  show (Var s)   = s
  show (And x y) = "and(" ++ show x ++ "," ++ show y ++ ")"
  show (Or  x y) =  "or(" ++ show x ++ "," ++ show y ++ ")"
  show (Neg x)   = "neg(" ++ show x ++ ")"

instance FromReqURI Exp where
  fromReqURI = parseHelper' parseExp "FromReqURI"

data ACondition = AC String Exp deriving Eq

instance Show ACondition where
  show (AC n e) = "ac(" ++ n ++ "," ++ show e ++ ")."

-- | Mechanisms for parsing:
parseExp :: Parsec String () Exp
parseExp = parseAnd <|> parseOr <|> parseNeg <|> parseVar

parseAc :: Parsec String () ACondition
parseAc = do
  string "ac("
  n <- many1 $ noneOf ","
  char ','
  e <- parseExp
  string ")."
  return $ AC n e

parseAcs :: Parsec String () [ACondition]
parseAcs = parseAc `sepBy` eol

parseHelper :: Parsec String () a -> SourceName -> String -> Either String a
parseHelper exp src content =
  case runP exp () src content of
    (Left err)   -> Left $ show err
    (Right keep) -> Right keep

parseHelper' :: Parsec String () a -> SourceName -> String -> Maybe a
parseHelper' e s c =
  case parseHelper e s c of
    (Left _)  -> Nothing
    (Right x) -> Just x

{- Parts to aid parsing that won't be exported: -}
parseAnd :: Parsec String () Exp
parseAnd = do
  string "and"
  (o1,o2) <- twoOps
  return $ And o1 o2

parseOr :: Parsec String () Exp
parseOr = do
  string "or"
  (o1,o2) <- twoOps
  return $ Or o1 o2

twoOps :: Parsec String () (Exp, Exp)
twoOps = do
  char '('
  o1 <- parseExp
  char ','
  o2 <- parseExp
  char ')'
  return (o1, o2)

parseNeg :: Parsec String () Exp
parseNeg = between (string "neg(") (char ')') $ liftM Neg parseExp

parseVar :: Parsec String () Exp
parseVar = liftM Var . many1 $ noneOf ",)"

eol :: Parsec String () String
eol = string "\n" <|> string "\n\r"

{-|
  Mechanisms to enable easier work with Exp:
|-}
idToExp :: IdType i => i -> Exp
idToExp = Var . show . unwrap . toId

renameVars :: Map String String -> Exp -> Exp
renameVars m (And e f) = And (renameVars m e) (renameVars m f)
renameVars m (Or e f)  = Or  (renameVars m e) (renameVars m f)
renameVars m (Neg e)   = Neg $ renameVars m e
renameVars m (Var n)   =
  case Map.lookup n m of
    (Just n') -> Var n'
    Nothing   -> Var n

renameAc :: Map String String -> ACondition -> ACondition
renameAc m (AC n e) =
  case Map.lookup n m of
    (Just n') -> AC n' $ renameVars m e
    Nothing   -> AC n  $ renameVars m e
