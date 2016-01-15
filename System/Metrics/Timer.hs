-- | This module defines a type for mutable, integer-valued timers.
-- Gauges are variable values and can be used to track e.g. the time
-- to complete a network request. All operations on timers are
-- thread-safe.
module System.Metrics.Timer
    (
      Timer
    , new
    , read
    , set
    ) where

import qualified Data.Atomic as Atomic
import Data.Int (Int64)
import Data.Time (NominalDiffTime)
import Prelude hiding (read)

-- | A mutable, integer-valued timer.
newtype Timer = T { unT :: Atomic.Atomic }

-- | Create a new, zero initialized, gauge.
new :: IO Timer
new = T `fmap` Atomic.new 0

-- | Get the current value of the timer.
read :: Timer -> IO Int64
read = Atomic.read . unT

-- | Set the current value of the timer.
set :: Timer -> NominalDiffTime -> IO ()
set timer = Atomic.write (unT timer) . microseconds
  -- statsd protocol mandates milliseconds.
  where microseconds = round . (* 1000)
