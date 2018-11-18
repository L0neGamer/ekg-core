{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a type for mutable dimensional string-valued label.
-- Dimensional label are variable values and can be used to track e.g. the
-- current number of call to an api endpoint by its http return code.
-- All operations on Dimensional label are thread-safe.
module System.Metrics.Dimensional where

import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label

import Control.Applicative ((<$>), pure)
import Control.Exception (throwIO)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import GHC.Exception (Exception)
import Prelude hiding (lookup)

type Name = Text

type Explanation = Text

type Dimensions = [Text]

type Point = [Text]

-- | Dimensional metrics storage
data Dimensional a = Dimensional
  { dimensionalCreate :: !(IO a)
  , dimensionalName :: !Name
  , dimensionalExplanation :: !Explanation
  , dimensionalDimensions :: !Dimensions
  , dimensionalPoints :: !(IORef (HashMap Point a))
  }

-- | Create a new empty dimensional metrics
newDimensional ::
     IO a -> Name -> Explanation -> Dimensions -> IO (Dimensional a)
newDimensional new name explanation dimensions =
  Dimensional new name explanation dimensions <$> newIORef empty

data DimensionError =
  DimensionError !Name
                 !Dimensions
                 !Point
  deriving (Show, Ord, Eq)

instance Exception DimensionError

data LookupFailure
  = UnmatchedDimensions DimensionError
  | NotFound
  deriving (Show, Ord, Eq)

-- | Returns dimensional metric with specified labels
lookup :: Dimensional a -> Point -> IO (Either LookupFailure a)
lookup d pt
  | not $ matchDimensions (dimensionalDimensions d) pt =
    pure $ Left (UnmatchedDimensions err)
  | otherwise = do
    toLookupResult . HashMap.lookup pt <$> readIORef (dimensionalPoints d)
  where
    err :: DimensionError
    err = DimensionError (dimensionalName d) (dimensionalDimensions d) pt
    toLookupResult Nothing = Left NotFound
    toLookupResult (Just x) = Right x

matchDimensions :: Dimensions -> Point -> Bool
matchDimensions ds ps = length ds == length ps

-- | Initialize a new empty dimensional metric with specified labels
create :: Dimensional a -> Point -> IO a
create d pt
  | not $ matchDimensions (dimensionalDimensions d) pt = throwIO err
  | otherwise = do
    v <- dimensionalCreate d
    atomicModifyIORef'
      (dimensionalPoints d)
      (\store -> (HashMap.insert pt v store, ()))
    return v
  where
    err :: DimensionError
    err = DimensionError (dimensionalName d) (dimensionalDimensions d) pt

-- | Returns dimensional metric with specified labels, creating it if not exists
lookupOrCreate :: Dimensional a -> Point -> IO a
lookupOrCreate d pt =
  lookup d pt >>= \case
    Left NotFound -> create d pt
    Left (UnmatchedDimensions err) -> throwIO err
    Right x -> return x

type Counter = Dimensional Counter.Counter

type Gauge = Dimensional Gauge.Gauge

type Label = Dimensional Label.Label

type Distribution = Dimensional Distribution.Distribution
