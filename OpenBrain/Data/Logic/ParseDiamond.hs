module OpenBrain.Data.Logic.ParseDiamond(
  DRParser
, parseDiamond
)where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Functor.Identity as Identity

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic.Diamond
import OpenBrain.Data.Logic.Parse

type Answer = (Int, DiamondResult String)
type DRParser a = MyParser (DiamondResult String) a

answer :: DRParser Answer
answer = do
  setState startState
  string "Answer: "
  num <- liftM read $ many1 digit
  eol
  (pieces `sepBy` char ' ') >> eol
  liftM ((,) num) getState
  where
    parsePiece :: String -> (String -> DiamondResult String -> DiamondResult String) -> DRParser ()
    parsePiece start update = between (string start >> char '(') (char ')') $ do
      name <- many1 $ noneOf ")"
      modifyState   $ update name

    parseIn   = parsePiece "in"   $ \s dr -> dr{inSet   = s:inSet   dr}
    parseUdec = parsePiece "udec" $ \s dr -> dr{udecSet = s:udecSet dr}
    parseOut  = parsePiece "out"  $ \s dr -> dr{outSet  = s:outSet  dr}

    pieces = choice [parseIn, parseUdec, parseOut]

answers :: DRParser [Answer]
answers = choice [found, finished, dropLine]
  where
    found = do
      a  <- answer
      as <- answers
      return $ a:as

    dropLine = do
      many $ noneOf "\r\n"
      eol >> answers

    finished =
      let models = void $ string "Models" >> many (noneOf "\r\n") >> eol
      in choice [eof, models] >> return []

rPart :: ResultType -> DRParser (ResultType, [DiamondResult String])
rPart rType = choice [found, dropLine, finished]
  where
    found = do
      string "Solving..." >> eol
      as <- answers
      return (rType, map snd as)

    dropLine = do
      many $ noneOf "\r\n"
      eol >> rPart rType

    finished =
      let models = void $ string "Models" >> many (noneOf "\r\n") >> eol
      in choice [eof, models] >> return (rType, []) 

parseDiamond :: DRParser (Results String)
parseDiamond = liftM Results $ mapM rPart [TwoValued, Stable, Admissible, Complete, Grounded]

test :: IO ()
test = do
  foo <- readFile "/tmp/foo"
  either putStrLn print $ execParser parseDiamond "/tmp/foo" foo
