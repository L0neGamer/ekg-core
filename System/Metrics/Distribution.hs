{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | This module defines a type for tracking statistics about a series
-- of events. An event could be handling of a request and the value
-- associated with the event -- the value you'd pass to 'add' -- could
-- be the amount of time spent serving the request. All operations are
-- thread safe.
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

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Data.Functor ((<$>))
import Data.Int (Int64)
import Prelude hiding (max, min, read, sum)
import qualified Prelude

-- TODO: Make a faster implementation.

-- | An metric for tracking events.
newtype Distribution = Distribution { unDistribution :: MVar State }

data State = State
    { sCount      :: !Int64
    , sMean       :: !Double
    , sSumSqDelta :: !Double
    , sSum        :: !Double
    , sMin        :: !Double
    , sMax        :: !Double
    }

dblMax :: Double
dblMax = 1.7976931348623157E+308

emptyState :: State
emptyState = State
    { sCount      = 0
    , sMean       = 0.0
    , sSumSqDelta = 0.0
    , sSum        = 0.0
    , sMin        = dblMax
    , sMax        = -dblMax
    }

-- | Create a new event tracker.
new :: IO Distribution
new = Distribution <$> newMVar emptyState

-- | Add a new value for the event.
add :: Distribution -> Double -> IO ()
add event val = addN event val 1

-- | Add multiple equal values for the event.
addN :: Distribution -> Double -> Int64 -> IO ()
addN event val n = modifyMVar_ (unDistribution event) $ \ State{..} ->
    -- Mean and variance are computed according to
    -- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
    let !count      = sCount + n
        !delta      = val - sMean
        !mean       = sMean + fromIntegral n * delta / fromIntegral count
        !sumSqDelta = sSumSqDelta + delta * (val - mean) * fromIntegral n
    in return $! State
        { sCount      = count
        , sMean       = mean
        , sSumSqDelta = sumSqDelta
        , sSum        = sSum + val
        , sMin        = Prelude.min val sMin
        , sMax        = Prelude.max val sMax
        }

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Stats
read event = do
    State{..} <- readMVar $ unDistribution event
    return $! Stats
        { mean  = sMean
        , variance = if sCount == 0 then 0.0
                     else sSumSqDelta / fromIntegral sCount
        , count = sCount
        , sum   = sSum
        , min   = sMin
        , max   = sMax
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
