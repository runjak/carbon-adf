{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Data.Logic.Parse(
  MyParser
, execParser
, execParser'
, StartState(..)
, changeState
, keepState
, eol
)where

import Control.Arrow (second)
import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import Text.Parsec as P
import Text.ParserCombinators.Parsec as PC
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map

type MyParser s a = ParsecT String s Identity.Identity a

execParser :: StartState s => MyParser s a -> SourceName -> String -> Either String a
execParser p src = either (Left . show) Right . Identity.runIdentity . runPT p startState src

execParser' :: MyParser () a -> SourceName -> String -> Either String a
execParser' = execParser

class StartState s where
  startState ::  s

instance StartState () where
  startState = ()

{-|
  This wonderful function came out of the StackOverflow question:
  http://stackoverflow.com/questions/17968784/an-easy-way-to-change-the-type-of-parsec-user-state
|-}
changeState :: forall m s u v a . (Functor m, Monad m)
            => (u -> v) -> (v -> u)
            -> ParsecT s u m a -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState :: forall u v . (u -> v) -> State s u -> State s v
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u v . (u -> v) -> Reply s u a -> Reply s v a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform :: (State s u -> m (Consumed (m (Reply s u a))))
              ->  State s v -> m (Consumed (m (Reply s v a)))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))

keepState :: forall m s u v a . (Functor m, Monad m, StartState u)
          => ParsecT s u m a -> ParsecT s v m a
keepState f = getState >>= \s -> changeState (const s) (const startState) f

{-| Helpful parser rudiments: |-}

eol :: MyParser s String
eol = string "\n" <|> string "\n\r"
