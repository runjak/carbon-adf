module Carbon.Data.Logic.ParseExp(
  parseExp
) where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import Carbon.Data.Logic.Exp
import Carbon.Data.Logic.Parse

parseExp :: MyParser s (Exp String)
parseExp = choice [parseAnd, parseOr, parseNeg, parseConst, parseVar]

parseAnd :: MyParser s (Exp String)
parseAnd = P.try $ do
  string "and"
  (o1,o2) <- twoOps
  return $ And o1 o2

parseOr :: MyParser s (Exp String)
parseOr = P.try $ do
  string "or"
  (o1,o2) <- twoOps
  return $ Or o1 o2

twoOps :: MyParser s (Exp String, Exp String)
twoOps = do
  char '('
  o1 <- parseExp
  char ','
  o2 <- parseExp
  char ')'
  return (o1, o2)

parseNeg :: MyParser s (Exp String)
parseNeg = P.try . between (string "neg(") (char ')') $ liftM Neg parseExp

parseConst :: MyParser s (Exp String)
parseConst = P.try $ do
  lookAhead $ string "c("
  between (string "c(") (char ')') $ do
    c <- oneOf "vVfF"
    let t = c `elem` "vV"
    return $ Const t

parseVar :: MyParser s (Exp String)
parseVar = liftM Var . many1 $ noneOf ",)"
