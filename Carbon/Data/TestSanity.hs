module Carbon.Data.TestSanity where
{-|
  This file gathers some cases where the Item should be considered sane, but somehow appears not to be.
|-}
import Data.Monoid
import qualified Data.Set as Set

import Carbon.Data.Item (Item (..))
import Carbon.Data.Description (Description (..))
import Carbon.Data.Discussion (Discussion (..), EvaluationState (..))
import qualified Carbon.Data.Item as Item

i1 :: Item String
i1 = Item {
  itemId      = mempty
, description = Just Description {
    descriptionId = mempty
  , headline      = "test1"
  , summary       = "One more time!"
  }
, article   = mempty
, condition = mempty
, relation  = mempty
, relations = mempty
, discussion = Just Discussion {
    discussionId = mempty
  , arguments    = Right Set.empty
  , deadline     = mempty
  , participants = Set.empty
  , evaluation   = NotEvaluated
  }
, resultSet = mempty
, creation  = mempty
, deletion  = mempty
, parents   = mempty
, children  = mempty
, commitMessage = mempty
, commitAuthor  = mempty
}

i2 :: Item String
i2 = Item {
  itemId      = mempty
, description = Just Description {
    descriptionId = Nothing
  , headline      = "Test 1"
  , summary       = "The next test."
  }
, article       = Nothing
, condition     = Nothing
, relation      = Nothing
, relations     = []
, discussion    = Nothing
, resultSet     = Nothing
, creation      = ""
, deletion      = Nothing
, parents       = []
, children      = []
, commitMessage = "General modification of the discussion."
, commitAuthor  = toEnum 2
}

items = [("i1", i1), ("i2", i2)]

test :: String -> Item String -> String
test name i
  | Item.itemIsSane i = "Item "++name++" is considered sane."
  | otherwise = "Item "++name++" is insane!"

main = putStrLn . unlines $ map (uncurry test) items
