module OpenBrain.Data.Logic(
  Exp(..), and', or'
, parseExp, parseAc, parseAcs
, parseHelper, parseHelper'
, idToExp
)where


import Control.Monad
import Happstack.Server (FromReqURI(..))
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC

import OpenBrain.Data.Id

data Exp = Var String
         | And Exp Exp
         | Or  Exp Exp
         | Not Exp
         deriving Eq

and' = foldr1 And
or'  = foldr1 Or

instance Show Exp where
  show (Var s)   = s
  show (And x y) = "and(" ++ show x ++ "," ++ show y ++ ")"
  show (Or  x y) =  "or(" ++ show x ++ "," ++ show y ++ ")"
  show (Not x)   = "not(" ++ show x ++ ")"

instance FromReqURI Exp where
  fromReqURI = parseHelper' parseExp "FromReqURI"

-- | Mechanisms for parsing:
parseExp :: Parsec String () Exp
parseExp = parseAnd <|> parseOr <|> parseNot <|> parseVar

parseAc :: Parsec String () Exp
parseAc = between (string "ac(") (string ").") $ parseExp

parseAcs :: Parsec String () [Exp]
parseAcs = parseAc `sepBy` eol

parseHelper :: (Parsec String () Exp) -> SourceName -> String -> Either String Exp
parseHelper exp src content =
  case runP exp () src content of
    (Left err)   -> Left $ show err
    (Right keep) -> Right keep

parseHelper' :: (Parsec String () Exp) -> SourceName -> String -> Maybe Exp
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

parseNot :: Parsec String () Exp
parseNot = between (string "not(") (char ')') $ liftM Not parseExp

parseVar :: Parsec String () Exp
parseVar = liftM (Var) . many1 $ noneOf ",)"

eol :: Parsec String () String
eol = string "\n" <|> string "\n\r"

{-|
  Mechanisms to enable easier work with Exp:
|-}
idToExp :: IdType i => i -> Exp
idToExp = Var . show . unwrap . toId
