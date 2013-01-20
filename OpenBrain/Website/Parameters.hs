module OpenBrain.Website.Parameters where
{-
  Definitions for different parameters parts of the website expect.
-}

import Happstack.Server as S
import System.Time (CalendarTime(..))
import Text.Regex as T
import qualified System.Time as Time

import OpenBrain.Data          hiding (getContent)
import OpenBrain.Website.Monad hiding (getContent)

getDisplay        = lookRead "display"                                    :: OBW Id
getLimit          = msum [lookRead "limit", return 30]                    :: OBW Limit
getOffset         = msum [lookRead "offset", return 0]                    :: OBW Offset
getAfter          = fetchCalendarTime
getDeadline       = do
  t      <- fetchCalendarTime
  future <- liftIO $ isFutureTime t
  guard future
  return t
getUser           = liftM fromId $ lookRead "user"                        :: OBW UserId
getItems          = liftM (map (fromId . wrap) . read) $ look "items"     :: OBW [InformationId]
getInformationId  = liftM fromId $ lookRead "informationId"               :: OBW InformationId
getTitle          = liftM sanitize $ look "title"                         :: OBW Title
getDescription    = liftM sanitize $ look "description"                   :: OBW Description
getContent        = liftM sanitize $ look "content"                       :: OBW Content
getTDC            = liftM3 (,,) getTitle getDescription getContent        :: OBW (Title, Description, Content)
getSplit          = msum [liftM (=="True") $ look "split", return False]  :: OBW Bool
getStatus         = msum [liftM (=="True") $ look "status", return False] :: OBW Bool -- | For setParticipant
getDiscussionType = lookRead "discussiontype"                             :: OBW DiscussionType

fetchCalendarTime :: OBW CalendarTime
fetchCalendarTime = do
  guard =<< liftM or (mapM (liftM (not . null) . look) ["year", "month", "day", "hour", "minute"])
  t      <- liftIO $ Time.toCalendarTime =<< Time.getClockTime
  year   <- msum [lookRead "year"  , return $ Time.ctYear t]
  month  <- msum [lookRead "month" , return . (+1) . fromEnum $ Time.ctMonth t]
  day    <- msum [lookRead "day"   , return $ Time.ctDay t]
  hour   <- msum [lookRead "hour"  , return $ Time.ctHour t]
  minute <- msum [lookRead "minute", return $ Time.ctMin t]
  return CalendarTime {
      ctYear    = year
    , ctMonth   = toEnum $ month - 1
    , ctDay     = day
    , ctHour    = hour
    , ctMin     = minute
    , ctSec     = 0
    , ctPicosec = 0
    , ctWDay    = toEnum 0
    , ctYDay    = 0
    , ctTZName  = "CET"
    , ctTZ      = 3600
    , ctIsDST   = False
    }

isFutureTime :: CalendarTime -> IO Bool
isFutureTime ct = do
  t <- Time.toCalendarTime =<< Time.getClockTime
  return $ ct > t

instance FromReqURI CalendarTime where
  fromReqURI s = case reads s of
    [(ct, _)] -> Just ct
    _ -> Nothing

instance FromReqURI DiscussionType where
  fromReqURI "AttackOnly"     = Just AttackOnly
  fromReqURI "AttackDefense"  = Just AttackDefense
  fromReqURI _ = Nothing

sanitize :: String -> String
sanitize = foldl1 (.) [
    \x -> subRegex (mkRegex "<") x "&lt;"
  , \x -> subRegex (mkRegex ">") x "&gt;"
  ]
