module OpenBrain.Data.Logic.ParseInstance(
  parseAc
, parseInstance
)where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import OpenBrain.Data.Logic.Exp
import OpenBrain.Data.Logic.Instance
import OpenBrain.Data.Logic.ParseExp

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
  when (i == emptyInstance) $
    fail "Parser result equals the empty instance."
  return i
