{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module for defining metrics that can be monitored.
--
-- Metrics are used to monitor program behavior and performance. All
-- metrics have
--
--  * a name, and
--
--  * a way to get the metric's current value.
--
-- This module provides a way to register metrics in a global \"metric
-- store\". The store can then be used to get a snapshot of all
-- metrics. The store also serves as a central place to keep track of
-- all the program's metrics, both user and library defined.
--
-- Here's an example of creating a single counter, used to count the
-- number of request served by a web server:
--
-- > import System.Metrics
-- > import qualified System.Metrics.Counter as Counter
-- >
-- > main = do
-- >     store <- newStore
-- >     requests <- createCounter "myapp.request_count" store
-- >     -- Every time we receive a request:
-- >     Counter.inc requests
--
-- This module also provides a way to register a number of predefined
-- metrics that are useful in most applications. See e.g.
-- 'registerGcMetrics'.
module System.Metrics
    (
      -- * Naming metrics
      -- $naming
      Tags
    , dimensional
    , unidimensional

      -- * The metric store
      -- $metric-store
    , Store
    , newStore

      -- * Registering metrics
      -- $registering
    , registerCounter
    , registerGauge
    , registerLabel
    , registerDistribution
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createLabel
    , createDistribution

      -- ** Predefined metrics
      -- $predefined
    , registerGcMetrics

      -- * Sampling metrics
      -- $sampling
    , Sample
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Label (Label)
import qualified System.Metrics.Label as Label

-- $naming
-- Compound metric names should be separated using underscores.
-- Example: @request_count@. Periods in the name imply namespacing.
-- Example: @\"myapp.users\"@. Some consumers of metrics will use
-- these namespaces to group metrics in e.g. UIs.
--
-- Libraries and frameworks that want to register their own metrics
-- should prefix them with a namespace, to avoid collision with
-- user-defined metrics and metrics defined by other libraries. For
-- example, the Snap web framework could prefix all its metrics with
-- @\"snap.\"@.
--
-- It's customary to suffix the metric name with a short string
-- explaining the metric's type e.g. using @\"_ms\"@ to denote
-- milliseconds.

------------------------------------------------------------------------
-- * The metric store

-- $metric-store
-- The metric store is a shared store of metrics. It allows several
-- disjoint components (e.g. libraries) to contribute to the set of
-- metrics exposed by an application. Libraries that want to provide a
-- set of metrics should defined a register method, in the style of
-- 'registerGcMetrics', that registers the metrics in the 'Store'. The
-- register function should document which metrics are registered and
-- their types (i.e. counter, gauge, label, or distribution).

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

type GroupId = Int

-- | The 'Store' state.
data State = State
     { stateMetrics :: !(M.HashMap Tags (Either MetricSampler GroupId))
     , stateGroups  :: !(IM.IntMap GroupSampler)
     , stateNextId  :: {-# UNPACK #-} !Int
     }

data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
     , groupSamplerMetrics :: !(M.HashMap Tags (a -> Value))
     }

-- TODO: Rename this to Metric and Metric to SampledMetric.
data MetricSampler = CounterS !(IO Int64)
                   | GaugeS !(IO Int64)
                   | LabelS !(IO T.Text)
                   | DistributionS !(IO Distribution.Stats)

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = do
    state <- newIORef $ State M.empty IM.empty 0
    return $ Store state

-- | A metric is identified by a non-empty, ordered list of tags.
--
-- The first tag conventionally is the counter name.
--
-- Note that the default 'IsString' instance uses a 'unidimensional' tag for
-- retro-compatibility and syntaxical reason.
newtype Tags = Tags { _tags :: NE.NonEmpty T.Text }
    deriving (Eq, Show, Hashable)

instance IsString Tags where
    fromString = unidimensional . fromString

-- | Creates a dimensional metric from a name tag and an ordered list of
-- dimensions.
dimensional :: T.Text -> [T.Text] -> Tags
dimensional n dims = Tags $ n NE.:| dims

-- | Creates a unidimensional metric with a single tag.
unidimensional :: T.Text -> Tags
unidimensional n = dimensional n []

------------------------------------------------------------------------
-- * Registering metrics

-- $registering
-- Before metrics can be sampled they need to be registered with the
-- metric store. The same metric name can only be used once. Passing a
-- metric name that has already been used to one of the register
-- function is an 'error'.

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter :: Tags      -- ^ Counter name
                -> IO Int64  -- ^ Action to read the current metric value
                -> Store     -- ^ Metric store
                -> IO ()
registerCounter name sample store =
    register name (CounterS sample) store

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge :: Tags      -- ^ Gauge name
              -> IO Int64  -- ^ Action to read the current metric value
              -> Store     -- ^ Metric store
              -> IO ()
registerGauge name sample store =
    register name (GaugeS sample) store

-- | Register a text metric. The provided action to read the value
-- must be thread-safe. Also see 'createLabel'.
registerLabel :: Tags       -- ^ Label name
              -> IO T.Text  -- ^ Action to read the current metric value
              -> Store      -- ^ Metric store
              -> IO ()
registerLabel name sample store =
    register name (LabelS sample) store

-- | Register a distribution metric. The provided action to read the
-- value must be thread-safe. Also see 'createDistribution'.
registerDistribution
    :: Tags                   -- ^ Distribution name
    -> IO Distribution.Stats  -- ^ Action to read the current metric
                              -- value
    -> Store                  -- ^ Metric store
    -> IO ()
registerDistribution name sample store =
    register name (DistributionS sample) store

register :: Tags
         -> MetricSampler
         -> Store
         -> IO ()
register name sample store = do
    atomicModifyIORef (storeState store) $ \ state@State{..} ->
        case M.member name stateMetrics of
            False -> let !state' = state {
                               stateMetrics = M.insert name
                                              (Left sample)
                                              stateMetrics
                             }
                     in (state', ())
            True  -> alreadyInUseError name

-- | Raise an exception indicating that the metric name is already in
-- use.
alreadyInUseError :: Tags   -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

-- | Register an action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in \"bulk\", as the run-time system provides a function to sample all
-- GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the provided
-- action computes @a@ atomically (e.g. if @a@ is a record, the action
-- needs to compute its fields atomically if the sampling is to be
-- atomic.)
--
-- Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.HashMap.Strict as M
-- > import GHC.Stats
-- > import System.Metrics
-- >
-- > main = do
-- >     store <- newStore
-- >     let metrics =
-- >             [ ("num_gcs", Counter . numGcs)
-- >             , ("max_bytes_used", Gauge . maxBytesUsed)
-- >             ]
-- >     registerGroup (M.fromList metrics) getGCStats store
registerGroup
    :: M.HashMap Tags
       (a -> Value)  -- ^ Metric names and getter functions.
    -> IO a          -- ^ Action to sample the metric group
    -> Store         -- ^ Metric store
    -> IO ()
registerGroup getters cb store = do
    atomicModifyIORef (storeState store) $ \ State{..} ->
        let !state' = State
                { stateMetrics = M.foldlWithKey' (register_ stateNextId)
                                 stateMetrics getters
                , stateGroups  = IM.insert stateNextId
                                 (GroupSampler cb getters)
                                 stateGroups
                , stateNextId  = stateNextId + 1
                }
       in (state', ())
  where
    register_ groupId metrics name _ = case M.lookup name metrics of
        Nothing -> M.insert name (Right groupId) metrics
        Just _  -> alreadyInUseError name

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a 'Counter') with registering that reference in the store in one
-- convenient function.

-- | Create and register a zero-initialized counter.
createCounter :: Tags    -- ^ Counter name
              -> Store   -- ^ Metric store
              -> IO Counter
createCounter name store = do
    counter <- Counter.new
    registerCounter name (Counter.read counter) store
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: Tags    -- ^ Gauge name
            -> Store   -- ^ Metric store
            -> IO Gauge
createGauge name store = do
    gauge <- Gauge.new
    registerGauge name (Gauge.read gauge) store
    return gauge

-- | Create and register an empty label.
createLabel :: Tags    -- ^ Label name
            -> Store   -- ^ Metric store
            -> IO Label
createLabel name store = do
    label <- Label.new
    registerLabel name (Label.read label) store
    return label

-- | Create and register an event tracker.
createDistribution :: Tags    -- ^ Distribution name
                   -> Store   -- ^ Metric store
                   -> IO Distribution
createDistribution name store = do
    event <- Distribution.new
    registerDistribution name (Distribution.read event) store
    return event

------------------------------------------------------------------------
-- * Predefined metrics

-- $predefined
-- This library provides a number of pre-defined metrics that can
-- easily be added to a metrics store by calling their register
-- function.

-- | Convert seconds to milliseconds.
toMs :: Double -> Int64
toMs s = round (s * 1000.0)

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- To enable GC statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.
--
-- Registered counters:
--
-- [@rts.gc.bytes_allocated@] Total number of bytes allocated
--
-- [@rts.gc.num_gcs@] Number of garbage collections performed
--
-- [@rts.gc.num_bytes_usage_samples@] Number of byte usage samples taken
--
-- [@rts.gc.cumulative_bytes_used@] Sum of all byte usage samples, can be
-- used with @numByteUsageSamples@ to calculate averages with
-- arbitrary weighting (if you are sampling this record multiple
-- times).
--
-- [@rts.gc.bytes_copied@] Number of bytes copied during GC
--
-- [@rts.gc.mutator_cpu_ms@] CPU time spent running mutator threads,
-- in milliseconds. This does not include any profiling overhead or
-- initialization.
--
-- [@rts.gc.mutator_wall_ms@] Wall clock time spent running mutator
-- threads, in milliseconds. This does not include initialization.
--
-- [@rts.gc.gc_cpu_ms@] CPU time spent running GC, in milliseconds.
--
-- [@rts.gc.gc_wall_ms@] Wall clock time spent running GC, in
-- milliseconds.
--
-- [@rts.gc.cpu_ms@] Total CPU time elapsed since program start, in
-- milliseconds.
--
-- [@rts.gc.wall_ms@] Total wall clock time elapsed since start, in
-- milliseconds.
--
-- Registered gauges:
--
-- [@rts.gc.max_bytes_used@] Maximum number of live bytes seen so far
--
-- [@rts.gc.current_bytes_used@] Current number of live bytes
--
-- [@rts.gc.current_bytes_slop@] Current number of bytes lost to slop
--
-- [@rts.gc.max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@rts.gc.peak_megabytes_allocated@] Maximum number of megabytes allocated
--
-- [@rts.gc.par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@rts.gc.par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
--
-- [@rts.gc.par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC. The ratio of
-- @par_tot_bytes_copied@ divided by @par_max_bytes_copied@ approaches
-- 1 for a maximally sequential run and approaches the number of
-- threads (set by the RTS flag @-N@) for a maximally parallel run.
registerGcMetrics :: Store -> IO ()
registerGcMetrics store =
    registerGroup
    (M.fromList
     [ ("rts.gc.bytes_allocated"          , Counter . Stats.bytesAllocated)
     , ("rts.gc.num_gcs"                  , Counter . Stats.numGcs)
     , ("rts.gc.num_bytes_usage_samples"  , Counter . Stats.numByteUsageSamples)
     , ("rts.gc.cumulative_bytes_used"    , Counter . Stats.cumulativeBytesUsed)
     , ("rts.gc.bytes_copied"             , Counter . Stats.bytesCopied)
     , ("rts.gc.mutator_cpu_ms"           , Counter . toMs . Stats.mutatorCpuSeconds)
     , ("rts.gc.mutator_wall_ms"          , Counter . toMs . Stats.mutatorWallSeconds)
     , ("rts.gc.gc_cpu_ms"                , Counter . toMs . Stats.gcCpuSeconds)
     , ("rts.gc.gc_wall_ms"               , Counter . toMs . Stats.gcWallSeconds)
     , ("rts.gc.cpu_ms"                   , Counter . toMs . Stats.cpuSeconds)
     , ("rts.gc.wall_ms"                  , Counter . toMs . Stats.wallSeconds)
     , ("rts.gc.max_bytes_used"           , Gauge . Stats.maxBytesUsed)
     , ("rts.gc.current_bytes_used"       , Gauge . Stats.currentBytesUsed)
     , ("rts.gc.current_bytes_slop"       , Gauge . Stats.currentBytesSlop)
     , ("rts.gc.max_bytes_slop"           , Gauge . Stats.maxBytesSlop)
     , ("rts.gc.peak_megabytes_allocated" , Gauge . Stats.peakMegabytesAllocated)
     , ("rts.gc.par_tot_bytes_copied"     , Gauge . gcParTotBytesCopied)
     , ("rts.gc.par_avg_bytes_copied"     , Gauge . gcParTotBytesCopied)
     , ("rts.gc.par_max_bytes_copied"     , Gauge . Stats.parMaxBytesCopied)
     ])
    getGcStats
    store

-- | Get GC statistics.
getGcStats :: IO Stats.GCStats
#if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

-- | Empty GC statistics, as if the application hasn't started yet.
emptyGCStats :: Stats.GCStats
# if MIN_VERSION_base(4,10,0)
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    , mblocksAllocated       = 0
    }
# else
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    }
# endif
#else
getGcStats = Stats.getGCStats
#endif

-- | Helper to work around rename in GHC.Stats in base-4.6.
gcParTotBytesCopied :: Stats.GCStats -> Int64
#if MIN_VERSION_base(4,6,0)
gcParTotBytesCopied = Stats.parTotBytesCopied
#else
gcParTotBytesCopied = Stats.parAvgBytesCopied
#endif

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | A sample of some metrics.
type Sample = M.HashMap Tags Value

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO Sample
sampleAll store = do
    state <- readIORef (storeState store)
    let metrics = stateMetrics state
        groups = stateGroups state
    cbSample <- sampleGroups $ IM.elems groups
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample
    return $! M.fromList allSamples

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO [(Tags , Value)]
sampleGroups cbSamplers = concat `fmap` sequence (map runOne cbSamplers)
  where
    runOne :: GroupSampler -> IO [(Tags , Value)]
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! map (\ (n, f) -> (n, f a)) (M.toList groupSamplerMetrics)

-- | The value of a sampled metric.
data Value = Counter {-# UNPACK #-} !Int64
           | Gauge {-# UNPACK #-} !Int64
           | Label {-# UNPACK #-} !T.Text
           | Distribution !Distribution.Stats
           deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Value
sampleOne (CounterS m)      = Counter <$> m
sampleOne (GaugeS m)        = Gauge <$> m
sampleOne (LabelS m)        = Label <$> m
sampleOne (DistributionS m) = Distribution <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: M.HashMap Tags (Either MetricSampler GroupId)
            -> IO [(Tags, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- M.toList m]) $ \ (name, ref) -> do
        val <- sampleOne ref
        return (name, val)
