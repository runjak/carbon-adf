module OpenBrain.Website.Parameters where
{-
  Definitions for different parameters parts of the website expect.
-}

import Happstack.Server as S
import System.Time (CalendarTime)
import Text.Regex as T

import OpenBrain.Backend.Types
import OpenBrain.Data.Id
import OpenBrain.Website.Monad

getDisplay        = lookRead "display"                                    :: OBW Id
getLimit          = msum [lookRead "limit", return 30]                    :: OBW Limit
getOffset         = msum [lookRead "offset", return 0]                    :: OBW Offset
getAfter          = lookRead "after"                                      :: OBW CalendarTime
getUser           = liftM fromId $ lookRead "user"                        :: OBW UserId
getItems          = liftM (map (fromId . wrap) . read) $ look "items"     :: OBW [InformationId]
getInformationId  = liftM fromId $ lookRead "informationId"               :: OBW InformationId
getTitle          = look "title"                                          :: OBW Title
getDescription    = look "description"                                    :: OBW Description
getContent        = liftM sanitize $ look "content"                       :: OBW Content
getTDC            = liftM3 (,,) getTitle getDescription getContent        :: OBW (Title, Description, Content)
getSplit          = msum [liftM (=="True") $ look "split", return False]  :: OBW Bool

instance FromReqURI CalendarTime where
  fromReqURI s = case reads s of
    [(ct, _)] -> Just ct
    _ -> Nothing

sanitize :: String -> String
sanitize = foldl1 (.) [
    \x -> subRegex (mkRegex "<") x "&lt;"
  , \x -> subRegex (mkRegex ">") x "&gt;"
  ]
