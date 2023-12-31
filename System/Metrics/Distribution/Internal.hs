{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Internal module used to share implementation details between the
-- family of ekg packages. DO NOT DEPEND ON THIS MODULE.
module System.Metrics.Distribution.Internal
    ( Stats(..)
    ) where

-- | Distribution statistics
data Stats = Stats
    { mean     :: !Double  -- ^ Sample mean
    , variance :: !Double  -- ^ Biased sample variance
    , count    :: !Int   -- ^ Event count
    , sum      :: !Double  -- ^ Sum of values
    , min      :: !Double  -- ^ Min value seen
    , max      :: !Double  -- ^ Max value seen
    } deriving (Eq, Show)
