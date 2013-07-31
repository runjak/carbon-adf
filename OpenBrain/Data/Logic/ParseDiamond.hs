module OpenBrain.Data.Logic.ParseDiamond(
  RParser
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

{-| Parsing DiamondResults: |-}
type DRParser a = MyParser (DiamondResult String) a

parseDR :: DRParser (DiamondResult String)
parseDR = do
  string "Answer: " >> many1 digit     >> eol
  pieces `sepBy` many1 (oneOf " \t") >> eol
  getState
  where
    parsePiece :: String -> (String -> DiamondResult String -> DiamondResult String) -> DRParser ()
    parsePiece start update = between (string start >> char '(') (char ')') $ do
      name <- many1 $ noneOf ")"
      modifyState   $ update name

    parseIn   = parsePiece "in"   $ \s dr -> dr{inSet   = s:inSet   dr}
    parseUdec = parsePiece "udec" $ \s dr -> dr{udecSet = s:udecSet dr}
    parseOut  = parsePiece "out"  $ \s dr -> dr{outSet  = s:outSet  dr}

    pieces = choice [parseIn, parseUdec, parseOut]

{-| Parsing Results: |-}
type RParser a = MyParser (Results String) a

parseDiamond :: RParser (Results String)
parseDiamond = do
  mapM_ parseResults [minBound ..]
  liftM (\(Results rs) -> Results $ reverse rs) getState

parseResults :: ResultType -> RParser ()
parseResults rType = do
  anyToken `manyTill` (string "Solving..." >> eol)
  dResults <- many $ keepState parseDR
  modifyState $ \(Results rs) -> Results $ (rType, dResults):rs
