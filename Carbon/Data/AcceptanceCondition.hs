{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.AcceptanceCondition where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import Carbon.Data.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Exp
import qualified Carbon.Data.Logic as Logic

data AcceptanceCondition leaf = AcceptanceCondition {
  acceptanceConditionId :: Maybe Id
, proofStandard         :: Maybe ProofStandard
, formula               :: Exp leaf
}

data ProofStandard = PSScintillaOfEvidence
                   | PSPreponderanceOfEvidence
                   | PSBeyondResonableDoubt
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Instances:
instance Eq l => Eq (AcceptanceCondition l) where
  a == b = let eqId = (==) `on` acceptanceConditionId
               eqPs = (==) `on` proofStandard
               eqF  = (==) `on` formula
           in (eqId a b && eqPs a b && eqF a b)

instance FromJSON (AcceptanceCondition String) where
  parseJSON (Object v) = do
    let setI ac i  = ac{acceptanceConditionId = Just i}
        setPS ac p = ac{proofStandard = Just p}
        setF ac f  = ac{formula = f}
        parseI ac  = msum [liftM (setI ac)  (v .: "id"),            return ac]
        parsePS ac = msum [liftM (setPS ac) (v .: "proofStandard"), return ac]
        parseF ac  = msum [liftM (setF ac)  (v .: "formula"),       return ac]
    ac <- parseF =<< parsePS =<< parseI mempty
    guard $ ac /= mempty
    return ac
  parseJSON _ = mzero

instance FromJSON (Exp String) where
  parseJSON (String s) = let e = Logic.execParser' Logic.parseExp "FromJSON" $ Text.unpack s
                         in case e of
                           (Right e) -> return e
                           (Left  f) -> fail f
  parseJSON _ = mzero

instance FromJSON ProofStandard where
  parseJSON (String "PSScintillaOfEvidence")     = return PSScintillaOfEvidence
  parseJSON (String "PSPreponderanceOfEvidence") = return PSPreponderanceOfEvidence
  parseJSON (String "PSBeyondResonableDoubt")    = return PSBeyondResonableDoubt
  parseJSON _ = mzero

instance Functor AcceptanceCondition where
  fmap f a = a{formula = fmap f (formula a)}

instance Insertable (AcceptanceCondition l) (Exp l) where
  c <+ f = c{formula = f}

instance Insertable (AcceptanceCondition l) Id where
  c <+ i = c{acceptanceConditionId = Just i}

instance Insertable (AcceptanceCondition l) ProofStandard where
  c <+ p = c{proofStandard = Just p}

instance Monoid (AcceptanceCondition leaf) where
  mempty = AcceptanceCondition {
      acceptanceConditionId = Nothing
    , proofStandard         = Nothing
    , formula               = Logic.Const False
    }
  mappend a b = AcceptanceCondition {
      acceptanceConditionId = (mappend `on` acceptanceConditionId) a b
    , proofStandard = proofStandard b
    , formula = formula b
    }

instance Monoid ProofStandard where
  mempty = minBound
  mappend = max

instance Ord l => Ord (AcceptanceCondition l) where
  compare a b = let cId = compare `on` acceptanceConditionId
                    cPs = compare `on` proofStandard
                    cF  = compare `on` formula
                    cs  = [cId a b, cPs a b, cF a b]
                in head (filter (/= EQ) cs ++ [EQ])

instance Show (AcceptanceCondition Id) where
  show = show . fmap show

instance Show (AcceptanceCondition String) where
  show a = concat [
      "AcceptanceCondition {proofStandard = "
    , show (proofStandard a)
    , ", formula = "
    , show (formula a)
    , "}"
    ]

instance ToJSON (AcceptanceCondition String) where
  toJSON a = object [
      "id" .= acceptanceConditionId a 
    , "proofStandard" .= proofStandard a
    , "formula" .= show (formula a)
    ]

instance ToJSON ProofStandard where
  toJSON = toJSON . show

instance VarContainer AcceptanceCondition where
  vars = vars . formula
