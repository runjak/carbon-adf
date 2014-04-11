module Carbon.Data.Logic.ParseInstance(
  parseAc
, parseInstance
)where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import Carbon.Data.Logic.Instance
import Carbon.Data.Logic.Parse
import Carbon.Data.Logic.ParseExp

type InstanceParser a = MyParser (Instance String) a

addCondition :: ACondition String -> InstanceParser (ACondition String)
addCondition c = do
  updateState $ \i -> i{conditions = c:conditions i}
  return c

addStatement :: Statement String -> InstanceParser (Statement String)
addStatement s = do
  updateState $ \i -> i{statements = s:statements i}
  return s

finish :: InstanceParser (Instance String)
finish = do
  i <- getState
  let as = reverse $ conditions i
      ss = reverse $ statements i
  return $ Instance as ss

parseAc :: InstanceParser (ACondition String)
parseAc = between (string "ac(") (string ").") $ do
  n <- many1 $ noneOf ","
  char ','
  e <- parseExp
  addCondition $ AC n e

parseSt :: InstanceParser (Statement String)
parseSt = between (string "s(") (string ").") $
  addStatement =<< (liftM Statement . many1 $ noneOf ")")

parseComment :: InstanceParser String
parseComment = between (char '%') eol . many $ noneOf "\n\r"

parseInstance :: InstanceParser (Instance String)
parseInstance = do
  many1 $ choice [void parseAc, void parseSt, void parseComment, void anyToken]
  i <- finish
  when (i == startState) $ fail "Parser result equals the empty instance."
  return i
