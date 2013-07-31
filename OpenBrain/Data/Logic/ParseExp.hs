module OpenBrain.Data.Logic.ParseExp(
  parseExp
) where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import OpenBrain.Data.Logic.Exp
import OpenBrain.Data.Logic.Parse

parseExp :: MyParser s Exp
parseExp = choice [parseAnd, parseOr, parseNeg, parseConst, parseVar]

parseAnd :: MyParser s Exp
parseAnd = do
  string "and"
  (o1,o2) <- twoOps
  return $ And o1 o2

parseOr :: MyParser s Exp
parseOr = do
  string "or"
  (o1,o2) <- twoOps
  return $ Or o1 o2

twoOps :: MyParser s (Exp, Exp)
twoOps = do
  char '('
  o1 <- parseExp
  char ','
  o2 <- parseExp
  char ')'
  return (o1, o2)

parseNeg :: MyParser s Exp
parseNeg = between (string "neg(") (char ')') $ liftM Neg parseExp

parseConst :: MyParser s Exp
parseConst = between (string "c(") (char ')') $ do
  c <- oneOf "vVfF"
  let t = c `elem` "vV"
  return $ Const t

parseVar :: MyParser s Exp
parseVar = liftM Var . many1 $ noneOf ",)"
