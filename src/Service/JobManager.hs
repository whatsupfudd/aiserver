module Service.JobManager where

import Control.Concurrent.STM (STM, atomically, readTQueue, writeTQueue
    , newTQueueIO, newTVarIO, writeTVar, readTVar, TVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent (forkIO, myThreadId, ThreadId)
import Control.Monad (forever, void, replicateM_)
import Control.Exception (SomeException, try)

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)


-- | Possible job states
data JobStatus = Pending | Running | Completed | Failed | Canceled
  deriving (Eq, Show)

-- | Internal job representation
data Job = Job {
    jobId      :: UUID
  , action     :: IO ()        -- ^ work to perform
  , statusVar  :: TVar JobStatus
  , threadVar  :: TVar (Maybe ThreadId)
  }

-- | Queue holding pending jobs
-- newtype JobQueue = JobQueue { unQueue :: TQueue Job }
type JobQueue = TQueue Job

-- | Initialize a job queue and start N workers
initJobQueue :: Int -> IO JobQueue
initJobQueue n = do
  q <- newTQueueIO
  replicateM_ n (workerLoop q)
  return q

-- | Worker thread loop
workerLoop :: JobQueue -> IO ()
workerLoop queue = void . forkIO . forever $ do
  job <- atomically $ readTQueue queue
  atomically $ writeTVar (statusVar job) Running
  tid <- myThreadId
  atomically $ writeTVar (threadVar job) (Just tid)
  result <- try (action job) :: IO (Either SomeException ())
  atomically $ writeTVar (threadVar job) Nothing
  atomically $ writeTVar (statusVar job) $
    case result of
      Right _ -> Completed
      Left _  -> Failed

-- | Submit a new job, returns its JobId
submitJob :: JobQueue -> IO () -> IO UUID
submitJob queue ioAction = do
  uuid <- nextRandom
  sVar <- newTVarIO Pending
  tVar <- newTVarIO Nothing
  let job = Job uuid ioAction sVar tVar
  atomically $ writeTQueue queue job
  return uuid

-- | Fetch current status of a job
fetchJobStatus :: JobQueue -> UUID -> STM (Maybe JobStatus)
fetchJobStatus queue jid = do
  -- For simplicity, scan all jobs; in production keep map of jobId->TVar
  return Nothing -- TODO: maintain Map inside TVar

-- | Cancel a running or pending job
cancelJob :: JobQueue -> UUID -> STM Bool
cancelJob queue jid = do
  -- TODO: locate job, if pending remove it, if running killThread
  return False

-- | Get simple stats: pending vs running
jobStats :: JobQueue -> STM (Int, Int)
jobStats queue = do
  -- TODO: track counts or scan
  return (0,0)
