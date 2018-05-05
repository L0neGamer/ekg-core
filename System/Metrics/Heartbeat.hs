{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable heartbeat times.
-- Heartbeats can be used to track the time elapsed since a critical
-- operation was last performed. All operations on heartbeasts are thread-safe.
module System.Metrics.Heartbeat
    (
      Heartbeat
    , new
    , read
    , set
    , reset
    ) where

import           Data.Int        (Int64)
import           Data.IORef      (IORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import           Prelude         hiding (read)

-- | A mutable time of last execution.
newtype Heartbeat = C (IORef UTCTime)

-- | Create a new heartbeat, with the time-of-last execution as the
-- creation time of the heartbeat.
new :: IO Heartbeat
new = do
  now <- getCurrentTime
  C `fmap` newIORef now

-- | Get the number of seconds since the last time the heartbeat was
-- reset.
read :: Heartbeat -> IO Int64
read (C ref) = do
  lastHeartbeat <- readIORef ref
  now           <- getCurrentTime
  let since     =  now `diffUTCTime` lastHeartbeat

  return $ round since

-- | Set the time of last execution to the given value.
set :: Heartbeat -> UTCTime -> IO ()
set (C ref) !t = atomicWriteIORef ref t

-- | Reset the heartbeat using the current time.
reset :: Heartbeat -> IO ()
reset heartbeat = getCurrentTime >>= set heartbeat
