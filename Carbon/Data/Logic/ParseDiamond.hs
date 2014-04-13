module Carbon.Data.Logic.ParseDiamond(
  Answer
, DRParser
, answers
)where

import Control.Monad
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Functor.Identity as Identity

import Carbon.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Diamond
import Carbon.Data.Logic.Parse

type Answer = DiamondResult String
type DRParser a = MyParser (DiamondResult String) a

answer :: DRParser Answer
answer = do
  setState startState
  (pieces `sepBy` char ' ') >> eol
  getState
  where
    parsePiece :: String -> (String -> DiamondResult String -> DiamondResult String) -> DRParser ()
    parsePiece start update = between (string start >> char '(') (char ')') $ do
      name <- many1 $ noneOf ")"
      modifyState   $ update name

    parseIn   = parsePiece "t" $ \s dr -> dr{inSet   = s:inSet   dr}
    parseUdec = parsePiece "u" $ \s dr -> dr{udecSet = s:udecSet dr}
    parseOut  = parsePiece "f" $ \s dr -> dr{outSet  = s:outSet  dr}

    pieces = choice [parseIn, parseUdec, parseOut]

answers :: DRParser [Answer]
answers = choice [found, dropLine, finished]
  where
    found = P.try $ do
      a  <- answer
      as <- answers
      return $ a:as

    dropLine = do
      many $ noneOf "\r\n"
      eol >> answers

    finished = eof >> return []

testAnswers :: FilePath -> IO ()
testAnswers path = do
  foo <- readFile path
  either putStrLn print $ execParser answers path foo
