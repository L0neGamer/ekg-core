{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
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
import Prelude hiding (max, min, read, sum)

import Foreign.Storable (sizeOf)

import GHC.Float
import GHC.Int
import GHC.IO
import GHC.Prim

import Data.Array
import System.Metrics.Distribution.Internal (Stats(..))
import System.Metrics.ThreadId

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: Array Stripe }

newtype Stripe = Stripe { stripeD :: Distrib }

data Distrib = Distrib (MutableByteArray## RealWorld)

distribLen :: Int
distribLen = (#size struct distrib)

lockPos :: Int
lockPos = div (#offset struct distrib, lock)
              (sizeOf (undefined :: Int))

countPos :: Int
countPos = div (#offset struct distrib, count)
               (sizeOf (undefined :: Int))

meanPos :: Int
meanPos = div (#offset struct distrib, mean)
              (sizeOf (undefined :: Double))

sumSqDeltaPos :: Int
sumSqDeltaPos = div (#offset struct distrib, sum_sq_delta)
                    (sizeOf (undefined :: Double))

sumPos :: Int
sumPos = div (#offset struct distrib, sum)
             (sizeOf (undefined :: Double))

minPos :: Int
minPos = div (#offset struct distrib, min)
             (sizeOf (undefined :: Double))

maxPos :: Int
maxPos = div (#offset struct distrib, max)
             (sizeOf (undefined :: Double))

newDistrib :: IO Distrib
newDistrib = IO $ \s ->
    case distribLen of { (I## distribLen') ->
    case newByteArray## distribLen' s of { (## s1, mba ##) ->
    case lockPos of { (I## lockPos') ->
    -- probably unecessary
    case atomicWriteIntArray## mba lockPos' 0## s1 of { s2 ->
    case countPos of { (I## countPos') ->
    case writeIntArray## mba countPos' 0## s2 of { s3 ->
    case meanPos of { (I## meanPos') ->
    case writeDoubleArray## mba meanPos' 0.0#### s3 of { s4 ->
    case sumSqDeltaPos of { (I## sumSqDeltaPos') ->
    case writeDoubleArray## mba sumSqDeltaPos' 0.0#### s4 of { s5 ->
    case sumPos of { (I## sumPos') ->
    case writeDoubleArray## mba sumPos' 0.0#### s5 of { s6 ->
    case minPos of { (I## minPos') ->
    case writeDoubleArray## mba minPos' 0.0#### s6 of { s7 ->
    case maxPos of { (I## maxPos') ->
    case writeDoubleArray## mba maxPos' 0.0#### s7 of { s8 ->
    (## s8, Distrib mba ##) }}}}}}}}}}}}}}}}

newStripe :: IO Stripe
newStripe = do
    d <- newDistrib
    return $! Stripe
        { stripeD = d
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

{-# INLINE spinLock #-}
spinLock :: MutableByteArray## RealWorld -> State## RealWorld -> State## RealWorld
spinLock mba = \s ->
    case lockPos of { (I## lockPos') ->
    case casIntArray## mba lockPos' 0## 1## s of { (## s1, r ##) ->
    case r ==## 0## of { 0## ->
    spinLock mba s1; _ -> s1 }}}

{-# INLINE spinUnlock #-}
spinUnlock :: MutableByteArray## RealWorld -> State## RealWorld -> State## RealWorld
spinUnlock mba = \s ->
    case lockPos of { (I## lockPos') ->
    case writeIntArray## mba lockPos' 0## s of { s2 ->
    s2 }}

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int -> IO ()
addN distribution (D## val) (I## n) = IO $ \s ->
    case myStripe distribution of { (IO myStripe') ->
    case myStripe' s of { (## s1, (Stripe (Distrib mba)) ##) ->
    case spinLock mba s1 of { s2 ->
    case countPos of { (I## countPos') ->
    case readIntArray## mba countPos' s2 of { (## s3, count ##) ->
    case meanPos of { (I## meanPos') ->
    case readDoubleArray## mba meanPos' s3 of { (## s4, mean ##) ->
    case sumSqDeltaPos of { (I## sumSqDeltaPos') ->
    case readDoubleArray## mba sumSqDeltaPos' s4 of { (## s5, sumSqDelta ##) ->
    case sumPos of { (I## sumPos') ->
    case readDoubleArray## mba sumPos' s5 of { (## s6, dSum ##) ->
    case minPos of { (I## minPos') ->
    case readDoubleArray## mba minPos' s6 of { (## s7, dMin ##) ->
    case maxPos of { (I## maxPos') ->
    case readDoubleArray## mba maxPos' s7 of { (## s8, dMax ##) ->
    case count +## n of { count' ->
    case val -#### mean of { delta ->
    case mean +#### ((int2Double## n) *#### delta /#### (int2Double## count')) of { mean' ->
    case sumSqDelta +#### (delta *#### (val -#### mean') *#### (int2Double## n)) of { sumSqDelta' ->
    case writeIntArray## mba countPos' count' s8 of { s9 ->
    case writeDoubleArray## mba meanPos' mean' s9 of { s10 ->
    case writeDoubleArray## mba sumSqDeltaPos' sumSqDelta' s10 of { s11 ->
    case writeDoubleArray## mba sumPos' (dSum +#### val) s11 of { s12 ->
    case (case val <#### dMin of { 0## -> dMin; _ -> val }) of { dMin' ->
    case (case val >#### dMax of { 0## -> dMax; _ -> val }) of { dMax' ->
    case writeDoubleArray## mba minPos' dMin' s12 of { s13 ->
    case writeDoubleArray## mba maxPos' dMax' s13 of { s14 ->
    case spinUnlock mba s14 of { s15 ->
    (## s15, () ##) }}}}}}}}}}}}}}}}}}}}}}}}}}}}

combine :: Distrib -> Distrib -> IO ()
combine (Distrib bMBA) (Distrib aMBA) = IO $ \s ->
    case spinLock bMBA s of { s1 ->
    case countPos of { (I## countPos') ->
    case readIntArray## aMBA countPos' s1 of { (## s2, aCount ##) ->
    case readIntArray## bMBA countPos' s2 of { (## s3, bCount ##) ->
    case aCount +## bCount of { count' ->
    case meanPos of { (I## meanPos' ) ->
    case readDoubleArray## aMBA meanPos' s3 of { (## s4, aMean ##) ->
    case readDoubleArray## bMBA meanPos' s4 of { (## s5, bMean ##) ->
    case bMean -#### aMean of { delta ->
    case (   (((int2Double## aCount) *#### aMean) +#### ((int2Double## bCount) *#### bMean))
         /#### (int2Double## count')
         ) of { mean' ->
    case sumSqDeltaPos of { (I## sumSqDeltaPos') ->
    case readDoubleArray## aMBA sumSqDeltaPos' s5 of { (## s6, aSumSqDelta ##) ->
    case readDoubleArray## bMBA sumSqDeltaPos' s6 of { (## s7, bSumSqDelta ##) ->
    case (   aSumSqDelta 
         +#### bSumSqDelta
         +#### (   delta
               *#### delta
               *#### (   (int2Double## aCount) *#### (int2Double## bCount)
                     /#### (int2Double## count')
                     )
               )
         ) of { sumSqDelta' ->
    case writeIntArray## aMBA countPos' count' s7 of { s8 ->
    case (case count' ==## 0## of { 0## -> mean'; _ -> 0.0#### }) of { writeMean ->
    case writeDoubleArray## aMBA meanPos' writeMean s8 of { s9 ->
    case writeDoubleArray## aMBA sumSqDeltaPos' sumSqDelta' s9 of { s10 ->
    case sumPos of { (I## sumPos') ->
    case readDoubleArray## aMBA sumPos' s10 of { (## s11, aSum ##) ->
    case readDoubleArray## bMBA sumPos' s11 of { (## s12, bSum ##) ->
    case writeDoubleArray## aMBA sumPos' (aSum +#### bSum) s12 of { s13 ->
    case minPos of { (I## minPos') ->
    case readDoubleArray## bMBA minPos' s13 of { (## s14, bMin ##) ->
    case writeDoubleArray## aMBA minPos' bMin s14 of { s15 ->
    case maxPos of { (I## maxPos') ->
    case readDoubleArray## bMBA maxPos' s15 of { (## s16, bMax ##) ->
    case writeDoubleArray## aMBA maxPos' bMax s16 of { s17 ->
    case spinUnlock bMBA s17 of { s18 ->
    (## s18, () ##) }}}}}}}}}}}}}}}}}}}}}}}}}}}}}

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Stats
read distrib = do
    result@(Distrib mba) <- newDistrib
    forM_ (toList $ unD distrib) $ \(Stripe d) ->
        combine d result
    IO $ \s ->
        case meanPos of { (I## meanPos') ->
        case countPos of { (I## countPos') ->
        case sumSqDeltaPos of { (I## sumSqDeltaPos') ->
        case sumPos of { (I## sumPos') ->
        case minPos of { (I## minPos') ->
        case maxPos of { (I## maxPos') ->
        case readIntArray## mba countPos' s of { (## s1, count ##) ->
        case readDoubleArray## mba meanPos' s1 of { (## s2, mean ##) ->
        case readDoubleArray## mba sumSqDeltaPos' s2 of { (## s3, sumSqDelta ##) ->
        case readDoubleArray## mba sumPos' s3 of { (## s4, dSum ##) ->
        case readDoubleArray## mba minPos' s4 of { (## s5, dMin ##) ->
        case readDoubleArray## mba maxPos' s5 of { (## s6, dMax ##) ->
        (## s6
        , Stats { mean = (D## mean)
                , variance = if (I## count) == 0 then 0.0
                             else (D## sumSqDelta) / (D## (int2Double## count))
                , count = (I## count)
                , sum = (D## dSum)
                , min = (D## dMin)
                , max = (D## dMax)
                }
        ##) }}}}}}}}}}}}
