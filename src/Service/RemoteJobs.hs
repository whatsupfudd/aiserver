
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.RemoteJobs where

import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode, LocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet
  ( initializeBackend, startMaster, startSlave, Backend, findSlaves )

import Network.Transport.TCP (createTransport, defaultTCPParameters, TCPAddr (..), defaultTCPAddr)

import GHC.Generics (Generic)
import Data.Binary (Binary)


data JobResult =
  JobSuccess String
  | JobFailure String
  deriving (Show, Generic)
instance Binary JobResult


-- | Initialize a local node with given host and port
initRemoteProcess :: String -> String -> IO LocalNode
initRemoteProcess host port = do
  Right transport <- createTransport (defaultTCPAddr host port) defaultTCPParameters
  newLocalNode transport initRemoteTable


remoteWorker :: String -> Process ()
remoteWorker aString = do
  parent <- getSelfPid
  result <- say aString
  send parent result

remotable ['remoteWorker]
myRemoteTable :: RemoteTable
myRemoteTable = Service.RemoteJobs.__remoteTable initRemoteTable

-- | Send a remote job (IO String) to a chosen node
--   Wraps the computation in a Process, sends it, and returns its ProcessId
sendRemoteJob
  :: NodeId
  -> Process String       -- ^ Job action
  -> Process ProcessId    -- ^ Returns pid of spawned job process
sendRemoteJob node action = spawn node ($(mkClosure 'remoteWorker) ("allo" :: String))
    -- remoteWorker: receives closure, runs action, sends back to parent

-- | Await result from a ProcessId with timeout
awaitResult
  :: ProcessId        -- ^ remote worker pid
  -> Int              -- ^ timeout in seconds
  -> Process (Maybe String)
awaitResult pid timeoutSec = do
  expectTimeout (timeoutSec * 1000000)


-- NOTE:
-- To test:
-- > backend <- initializeBackend "127.0.0.1" "8080" initRemoteTable
-- > slaves <- findSlaves backend
-- > startMaster backend $ \slaves -> do
-- >   node <- head slaves
-- >   pid  <- sendRemoteJob node (return "Hello from remote")
-- >   res  <- awaitResult pid 5
-- >   liftIO $ print res
