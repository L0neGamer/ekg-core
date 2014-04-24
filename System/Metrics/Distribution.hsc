{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

#include "distrib.h"

-- | This module defines a type for tracking statistics about a series
-- of events. An event could be handling of a request and the value
-- associated with the event -- the value you'd pass to 'add' -- could
-- be the amount of time spent serving that request (e.g. in
-- milliseconds). All operations are thread safe.
module System.Metrics.Distribution
    ( Distribution
    , new
    , add
    , addN
    , read

      -- * Gathered statistics
    , Stats
    , mean
    , variance
    , count
    , sum
    , min
    , max
    ) where

import Control.Monad (forM_, replicateM)
import Data.Int (Int64)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(alignment, peek, poke, sizeOf), peekByteOff,
                         pokeByteOff)
import Prelude hiding (max, min, read, sum)

import Data.Array
import qualified Data.Mutex as Mutex
import System.Metrics.ThreadId

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: Array Stripe }

data Stripe = Stripe
    { stripeFp    :: !(ForeignPtr CDistrib)
    , stripeMutex :: !Mutex.Mutex
    }

-- | Perform action with lock held. Not exception safe.
withMutex :: Mutex.Mutex -> IO a -> IO a
withMutex lock m = do
    Mutex.lock lock
    a <- m
    Mutex.unlock lock
    return a

data CDistrib = CDistrib
    { cCount      :: !Int64
    , cMean       :: !Double
    , cSumSqDelta :: !Double
    , cSum        :: !Double
    , cMin        :: !Double
    , cMax        :: !Double
    }

instance Storable CDistrib where
    sizeOf _ = (#size struct distrib)
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        cCount <- (#peek struct distrib, count) p
        cMean <- (#peek struct distrib, mean) p
        cSumSqDelta <- (#peek struct distrib, sum_sq_delta) p
        cSum <- (#peek struct distrib, sum) p
        cMin <- (#peek struct distrib, min) p
        cMax <- (#peek struct distrib, max) p
        return $! CDistrib
            { cCount      = cCount
            , cMean       = cMean
            , cSumSqDelta = cSumSqDelta
            , cSum        = cSum
            , cMin        = cMin
            , cMax        = cMax
            }

    poke p CDistrib{..} = do
        (#poke struct distrib, count) p cCount
        (#poke struct distrib, mean) p cMean
        (#poke struct distrib, sum_sq_delta) p cSumSqDelta
        (#poke struct distrib, sum) p cSum
        (#poke struct distrib, min) p cMin
        (#poke struct distrib, max) p cMax

newCDistrib :: IO (ForeignPtr CDistrib)
newCDistrib = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p $ CDistrib 0 0.0 0.0 0.0 0.0 0.0
    return fp

newStripe :: IO Stripe
newStripe = do
    fp <- newCDistrib
    mutex <- Mutex.new
    return $! Stripe
        { stripeFp    = fp
        , stripeMutex = mutex
        }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Stripe
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `index` (tid `mod` numStripes)

------------------------------------------------------------------------
-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . fromList numStripes) `fmap`
      replicateM numStripes newStripe

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

foreign import ccall unsafe "hs_distrib_add_n" cDistribAddN
    :: Ptr CDistrib -> Double -> Int64 -> IO ()

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int64 -> IO ()
addN distrib val n = do
    stripe <- myStripe distrib
    withForeignPtr (stripeFp stripe) $ \ p ->
        withMutex (stripeMutex stripe) $ cDistribAddN p val n

foreign import ccall unsafe "hs_distrib_combine" combine
    :: Ptr CDistrib -> Ptr CDistrib -> IO ()

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Stats
read distrib = do
    result <- newCDistrib
    CDistrib{..} <- withForeignPtr result $ \ resultp -> do
        forM_ (toList $ unD distrib) $ \ stripe ->
            withForeignPtr (stripeFp stripe) $ \ p ->
            withMutex (stripeMutex stripe) $
            combine p resultp
        peek resultp
    return $! Stats
        { mean  = cMean
        , variance = if cCount == 0 then 0.0
                     else cSumSqDelta / fromIntegral cCount
        , count = cCount
        , sum   = cSum
        , min   = cMin
        , max   = cMax
        }

-- | Distribution statistics
data Stats = Stats
    { mean     :: !Double  -- ^ Sample mean
    , variance :: !Double  -- ^ Biased sample variance
    , count    :: !Int64   -- ^ Event count
    , sum      :: !Double  -- ^ Sum of values
    , min      :: !Double  -- ^ Min value seen
    , max      :: !Double  -- ^ Max value seen
    } deriving Show
