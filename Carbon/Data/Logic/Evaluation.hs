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
import Control.Concurrent.STM
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Process as Process

import Carbon.Config
import Carbon.Data.Logic.Diamond
import qualified Carbon.Data.Logic as Logic

-- | TODO decide if we need to delete the file.
run :: Config -> FilePath -> String -> IO (Results String)
run conf path content = do
  let rTypes = diamondEval conf
  writeFile path content
  state <- mkState $ length rTypes
  watchDog state
  mapM_ (runSingle conf state path) rTypes
  liftM merge $ awaitFinish state
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
      ts <- atomically $ do
        writeTVar (tCount state) 0
        readTVar (threads state)
      mapM_ killThread ts

    -- | Waits till tCount is 0 and returns all results.
    awaitFinish :: State -> IO [Results String]
    awaitFinish state = atomically $ do
      tc <- readTVar $ tCount state
      check $ tc <= 0
      readTVar $ results state

-- | Running a single evaluation:
runSingle :: Config -> State -> FilePath -> ResultType -> IO ()
runSingle conf state path rType = void . forkIO $ do
  tId <- myThreadId
  atomically $ modifyTVar (threads state) (tId:)
  let call = diamondCall conf
      parm = diamondParams conf Map.! rType ++ [path]
      proc = Process.proc call parm
  dOutput <- Process.readProcess call parm ""
  either (onError tId) (onResult tId) $ Logic.execParser (Logic.parseDiamond rType) path dOutput
  where
    onError :: ThreadId -> String -> IO ()
    onError tId e = do
      atomically $ do
        modifyTVar (threads state) (filter (tId /=))
        modifyTVar (tCount state) $ subtract 1
      mapM_ putStrLn ["Could not parse diamond output for discussion \""++path++"\":",e]

    onResult :: ThreadId -> Results String -> IO ()
    onResult tId r = atomically $ do
      modifyTVar (threads state) $ filter (tId /=)
      modifyTVar (tCount  state) $ subtract 1
      modifyTVar (results state) (r :)

-- | The shated state for all threads:
data State = State {
    threads :: TVar [ThreadId]
  , tCount  :: TVar Int
  , results :: TVar [Results String]
}

-- | Initialising the shared state:
mkState :: Int -> IO State
mkState i = atomically $ do
  ts <- newTVar []
  tc <- newTVar i
  rs <- newTVar []
  return State {
      threads = ts
    , tCount  = tc
    , results = rs
    }
