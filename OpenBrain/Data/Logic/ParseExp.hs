module OpenBrain.Data.Logic.ParseExp(
  parseExp, eol
) where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import OpenBrain.Data.Logic.Exp
import OpenBrain.Data.Logic.Instance

parseExp :: InstanceParser Exp
parseExp = choice [parseAnd, parseOr, parseNeg, parseConst, parseVar]

parseAnd :: InstanceParser Exp
parseAnd = do
  string "and"
  (o1,o2) <- twoOps
  return $ And o1 o2

parseOr :: InstanceParser Exp
parseOr = do
  string "or"
  (o1,o2) <- twoOps
  return $ Or o1 o2

twoOps :: InstanceParser (Exp, Exp)
twoOps = do
  char '('
  o1 <- parseExp
  char ','
  o2 <- parseExp
  char ')'
  return (o1, o2)

parseNeg :: InstanceParser Exp
parseNeg = between (string "neg(") (char ')') $ liftM Neg parseExp

parseConst :: InstanceParser Exp
parseConst = between (string "c(") (char ')') $ do
  c <- oneOf "vVfF"
  let t = c `elem` "vV"
  return $ Const t

parseVar :: InstanceParser Exp
parseVar = liftM Var . many1 $ noneOf ",)"

eol :: InstanceParser String
eol = string "\n" <|> string "\n\r"
