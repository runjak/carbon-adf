module OpenBrain.Deadline (
  DState, TDState, Deadline,
  newState, abort, enqueue
) where
{-|
  This module implements eventhandling for deadlines from discussions.
  The closest deadline is kept via enqueue and if it passes an IO () is performed.
  The Deadline Monad is used in OpenBrain.Website.Monad and lifted into the OBW Monad,
  because the Deadline State is also part of the WebsiteState.
|-}

import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified System.Time as Time

{- The Deadline State: -}
data DState = DState {
    deadline :: Time.CalendarTime
  , sleeper  :: Concurrent.ThreadId
  } | NoState
{- DState has to be handled atomically: -}
type TDState = STM.TVar DState
{- We use a StateTransformer to define the Deadline Monad: -}
type Deadline = StateT TDState IO

newState :: IO TDState
newState = STM.atomically $ STM.newTVar NoState

abort :: Deadline ()
abort = get >>= \s -> do
  mKill <- liftIO $ STM.atomically $ do
    s' <- STM.readTVar s
    STM.writeTVar s NoState
    case s' of
      NoState -> return Nothing
      ds@_    -> return . Just $ sleeper ds
  when (isJust mKill) . liftIO . Concurrent.killThread $ fromJust mKill

sleepUntil :: Time.CalendarTime -> IO ()
sleepUntil t = do
  t' <- Time.getClockTime
  let diff  = Time.diffClockTimes (Time.toClockTime t) t'
      sleep = Time.tdSec diff * 1000000
  when (sleep > 0) $ Concurrent.threadDelay sleep

perform :: IO () -> Time.CalendarTime -> Deadline ()
perform action time = abort >> get >>= \s -> do
  tid <- liftIO . Concurrent.forkIO $ do
    sleepUntil time
    STM.atomically $ STM.writeTVar s NoState
    void $ Concurrent.forkIO action
  liftIO . STM.atomically . STM.writeTVar s $ DState time tid

useTime :: Time.CalendarTime -> Deadline Bool
useTime time = get >>= \s -> do
  s' <- liftIO . STM.atomically $ STM.readTVar s
  case s' of
    NoState -> return True
    ds@_    -> return . (time <) $ deadline ds

enqueue :: IO () -> Time.CalendarTime -> Deadline Bool
enqueue action time = do
  use <- useTime time
  when use $ perform action time
  return use

