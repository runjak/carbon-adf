{-# LANGUAGE CPP #-}
module Carbon.Data.Logic.Evaluation(
  run
)where
{-
  This module aims for concurrent evaluation of a discussion.
  The entry point is the run function.
  Evaluation is performed as followes:
  1.: The Content is written into a temporary file
  2.: Diamond is called on the file for each ResultType concurrently
  3.: Each of the concurrent threads parses it's diamond output
  4.: The concurrent outputs are gathered and returned.
-}
import Control.Concurrent
import Control.Concurrent.STM (TVar, STM)
import Control.Monad
import Data.Map (Map)
import System.Directory (removeFile)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified System.Process as Process

import Carbon.Config
import Carbon.Data.Logic.Diamond
import qualified Carbon.Data.Logic as Logic

#if MIN_VERSION_stm(2, 3, 0)
modifyTVar = STM.modifyTVar
#else
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar t f = STM.writeTVar t =<< liftM f (STM.readTVar t)
#endif

run :: Config -> FilePath -> String -> IO (Results String)
run conf path content = do
  let rTypes = diamondEval conf
  writeFile path content
  state <- mkState $ length rTypes
  watchDog state
  mapM_ (runSingle conf state path) rTypes
  results <- liftM merge $ awaitFinish state
  when (deleteAfterEval conf) $ removeFile path
  return results
  where
    merge :: [Results String] -> Results String
    merge = Results . Map.toList . foldl go Map.empty
      where
        go :: Map ResultType [DiamondResult String] -> Results String -> Map ResultType [DiamondResult String]
        go m (Results rs) = foldl (flip $ uncurry (Map.insertWith' (++))) m rs

    -- | Sleeps some time and sets tCount to 0 afterwards.
    watchDog :: State -> IO ()
    watchDog state = void . forkIO $ do
      let second = 1000000
      threadDelay $ 10 * second
      ts <- STM.atomically $ do
        STM.writeTVar (tCount state) 0
        STM.readTVar (threads state)
      mapM_ killThread ts

    -- | Waits till tCount is 0 and returns all results.
    awaitFinish :: State -> IO [Results String]
    awaitFinish state = STM.atomically $ do
      tc <- STM.readTVar $ tCount state
      STM.check $ tc <= 0
      STM.readTVar $ results state

-- | Running a single evaluation:
runSingle :: Config -> State -> FilePath -> ResultType -> IO ()
runSingle conf state path rType = void . forkIO $ do
  tId <- myThreadId
  STM.atomically $ modifyTVar (threads state) (tId:)
  let call = diamondCall conf
      parm = diamondParams conf Map.! rType ++ [path]
      proc = Process.proc call parm
  dOutput <- Process.readProcess call parm ""
  either (onError tId) (onResult tId rType) $ Logic.execParser Logic.answers path dOutput
  where
    onError :: ThreadId -> String -> IO ()
    onError tId e = do
      STM.atomically $ do
        modifyTVar (threads state) (filter (tId /=))
        modifyTVar (tCount state) $ subtract 1
      mapM_ putStrLn ["Could not parse diamond output for discussion \""++path++"\":",e]

    onResult :: ThreadId -> ResultType -> [Logic.Answer] -> IO ()
    onResult tId rType answers = STM.atomically $ do
      let r = Results [(rType, answers)]
      modifyTVar (threads state) $ filter (tId /=)
      modifyTVar (tCount  state) $ subtract 1
      modifyTVar (results state) (r :)

-- | The shared state for all threads:
data State = State {
    threads :: TVar [ThreadId]
  , tCount  :: TVar Int
  , results :: TVar [Results String]
}

-- | Initialising the shared state:
mkState :: Int -> IO State
mkState i = STM.atomically $ do
  ts <- STM.newTVar []
  tc <- STM.newTVar i
  rs <- STM.newTVar []
  return State {
      threads = ts
    , tCount  = tc
    , results = rs
    }
