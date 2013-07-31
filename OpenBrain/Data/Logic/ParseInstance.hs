module OpenBrain.Data.Logic.ParseInstance(
  parseAc
, parseInstance
)where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import OpenBrain.Data.Logic.Instance
import OpenBrain.Data.Logic.Parse
import OpenBrain.Data.Logic.ParseExp

type InstanceParser a = MyParser Instance a

addCondition :: ACondition -> InstanceParser ACondition
addCondition c = do
  updateState $ \i -> i{conditions = c:conditions i}
  return c

addStatement :: Statement -> InstanceParser Statement
addStatement s = do
  updateState $ \i -> i{statements = s:statements i}
  return s

finish :: InstanceParser Instance
finish = do
  i <- getState
  let as = reverse $ conditions i
      ss = reverse $ statements i
  return $ Instance as ss

parseAc :: InstanceParser ACondition
parseAc = between (string "ac(") (string ").") $ do
  n <- many1 $ noneOf ","
  char ','
  e <- parseExp
  addCondition $ AC n e

parseSt :: InstanceParser Statement
parseSt = between (string "statement(") (string ").") $
  addStatement =<< (liftM Statement . many1 $ noneOf ")")

parseComment :: InstanceParser String
parseComment = between (char '%') eol . many $ noneOf "\n\r"

parseInstance :: InstanceParser Instance
parseInstance = do
  many1 $ choice [void parseAc, void parseSt, void parseComment, void eol, void spaces]
  i <- finish
  when (i == startState) $ fail "Parser result equals the empty instance."
  return i
