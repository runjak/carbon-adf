{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.Item where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Carbon.Data.Common
import Carbon.Data.AcceptanceCondition
import Carbon.Data.Alias
import Carbon.Data.Article
import Carbon.Data.Description
import Carbon.Data.Discussion
import Carbon.Data.Id
import Carbon.Data.Logic.Exp
import Carbon.Data.Relation
import Carbon.Data.ResultSet

data Item leaf = Item {
  itemId :: Id
-- | Changing the Data below creates a new version.
, description :: Maybe Description
, article     :: Maybe Article
, condition   :: Maybe (AcceptanceCondition leaf)
, relation    :: Maybe Relation
, relations   :: [Item Id]
, discussion  :: Maybe (Discussion (Item leaf))
, resultSet   :: Maybe ResultSet -- | Added each time the Items Discussion is evaluated.
-- | Data below is used for versioning of Items
, creation      :: Timestamp
, deletion      :: Maybe Timestamp
, parents       :: [Id]
, children      :: [Id]
, commitMessage :: String
, commitAuthor  :: Id
}

-- Predicates for Items:
itemIsDescription :: Item a -> Bool
itemIsDescription = go . description
  where
    go Nothing = False
    go (Just d) = mempty /= headline d

itemIsArticle :: Item a -> Bool
itemIsArticle i = itemIsDescription i && go (article i)
  where
    go Nothing = False
    go (Just a) = mempty /= content a

itemIsCondition :: Item a -> Bool
itemIsCondition i = itemIsDescription i && Maybe.isJust (condition i) && Maybe.isNothing (relation i)

itemIsRelation :: Item a -> Bool
itemIsRelation i = itemIsDescription i && Maybe.isJust (relation i) && Maybe.isNothing (condition i)

itemIsDiscussion :: Item a -> Bool
itemIsDiscussion i = itemIsDescription i && Maybe.isJust (discussion i)

itemIsResult :: Item a -> Bool
itemIsResult i = itemIsDiscussion i && Maybe.isJust (resultSet i)

itemIsSane :: Item a -> Bool
itemIsSane i = or [itemIsArticle i, itemIsCondition i, itemIsRelation i, itemIsDiscussion i]

-- Instances, alphabetical order:
instance Eq (Item i) where
  (==) = sameId

instance FromJSON (Item Id) where
  parseJSON = liftM (fmap read) . parseJSON

instance FromJSON (Item String) where
  parseJSON (Object v) = Item
                     <$> v .: "id"
                     <*> v .: "description"
                     <*> v .: "article"
                     <*> v .: "condition"
                     <*> v .: "relation"
                     <*> v .: "relations"
                     <*> v .: "discussion"
                     <*> v .: "resultSet"
                     <*> v .: "creation"
                     <*> v .: "deletion"
                     <*> v .: "parents"
                     <*> v .: "children"
                     <*> v .: "commitMessage"
                     <*> v .: "commitAuthor"
  parseJSON _ = mzero

instance Functor Item where
  fmap f i = i{condition = nCondition i, discussion = nDiscussion i}
    where
      nCondition  = fmap (fmap f) . condition
      nDiscussion = fmap (mapItems f) . discussion

instance HasId (Item i) where
  getId = itemId

instance Insertable (Item Id) (AcceptanceCondition Id) where
  i <+ a = i{condition = Just a}

instance Insertable (Item Id) Article where
  i <+ a = i{article = Just a}

instance Insertable (Item Id) Description where
  i <+ d = i{description = Just d}

instance Insertable (Item Id) (Discussion (Item Id)) where
  i <+ d = i{discussion = Just d}

instance Insertable (Item Id) Relation where
  i <+ r = i{relation = Just r}

instance Insertable (Item Id) ResultSet where
  i <+ r = i{resultSet = Just r}

instance Insertable (Item leaf) Id where
  i <+ iid = i{itemId = iid}

instance Insertable (Item leaf) [Item Id] where
  i <+ rs = i{relations = rs}

instance Monoid (Item leaf) where
  mempty = Item {
      itemId        = mempty
    , description   = mempty
    , article       = mempty
    , condition     = mempty
    , relation      = mempty
    , relations     = mempty
    , discussion    = mempty
    , resultSet     = mempty
    , creation      = mempty
    , deletion      = mempty
    , parents       = mempty
    , children      = mempty
    , commitMessage = mempty
    , commitAuthor  = mempty
    }
  mappend a b = Item {
      itemId        = (mappend `on` itemId) a b
    , description   = (mappend `on` description) a b
    , article       = (mappend `on` article) a b
    , condition     = (mappend `on` condition) a b
    , relation      = (mappend `on` relation) a b
    , relations     = relations b
    , discussion    = (mappend `on` discussion) a b
    , resultSet     = (mappend `on` resultSet) a b
    , creation      = (mergeStr `on` creation) a b
    , deletion      = deletion b
    , parents       = parents b
    , children      = children b
    , commitMessage = (mergeStr `on` commitMessage) a b
    , commitAuthor  = (mappend `on` commitAuthor) a b
    }

instance Ord (Item i) where
  compare = compareId

instance Show (Item Id) where
  show = show . fmap show

instance Show (Item String) where
  show i = concat [
      "Item {itemId = "
    , show (itemId i)
    , ", description = "
    , show (description i)
    , ", article = "
    , show (article i)
    , ", condition = "
    , show (condition i)
    , ", relation = "
    , show (relation i)
    , ", relations = "
    , show (relations i)
    , ", discussion = "
    , show (discussion i)
    , ", resultSet = "
    , show (resultSet i)
    , ", creation = "
    , show (creation i)
    , ", deletion = "
    , show (deletion i)
    , ", parents = "
    , show (parents i)
    , ", children = "
    , show (children i)
    , ", commitMessage = "
    , show (commitMessage i)
    , ", commitAuthor = "
    , show (commitAuthor i)
    , "}"
    ]

instance ToJSON (Item Id) where
  toJSON = toJSON . fmap show

instance ToJSON (Item String) where
  toJSON i = object [
      "id"            .= itemId i
    , "description"   .= description i
    , "article"       .= article i
    , "condition"     .= condition i
    , "relation"      .= relation i
    , "relations"     .= relations i
    , "discussion"    .= discussion i
    , "resultSet"     .= resultSet i
    , "creation"      .= creation i
    , "deletion"      .= deletion i
    , "parents"       .= parents i
    , "children"      .= children i
    , "commitMessage" .= commitMessage i
    , "commitAuthor"  .= commitAuthor i
    ]

instance VarContainer Item where
  vars = go . condition
    where
      go  Nothing = Set.empty
      go (Just a) = vars a

-- A little debug help:
sanityTable :: Item String -> IO ()
sanityTable i = do
  putStrLn "Carbon.Data.Item:sanityTable:"
  putStrLn "-----------------------------"
  let stats = [("Description: ", itemIsDescription i)
              ,("Article:     ", itemIsArticle     i)
              ,("Condition:   ", itemIsCondition   i)
              ,("Relation:    ", itemIsRelation    i)
              ,("Discussion:  ", itemIsDiscussion  i)
              ,("Result:      ", itemIsResult      i)
              ,("Sane:        ", itemIsSane        i)
              ]
  forM_ stats $ \(x,y) -> putStr x >> print y
  putStrLn "-----------------------------"
  putStrLn "Item contents:"
  print i
