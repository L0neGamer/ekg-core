{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Metrics.Dimensional where

import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label

import Prelude hiding (lookup)
import GHC.Exception (Exception)
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HashMap

type Name = Text
type Explanation = Text
type Dimensions = [Text]
type Point = [Text]

data Dimensional a = Dimensional {
    _create      :: !(IO a)
  , _name        :: !Name
  , _explanation :: !Explanation
  , _dimensions  :: !Dimensions
  , _points      :: !(IORef (HashMap Point a))
  }

newDimensional :: IO a -> Name -> Explanation -> Dimensions -> IO (Dimensional a)
newDimensional new name explanation dimensions =
    Dimensional new name explanation dimensions <$> newIORef empty

data DimensionError
  = DimensionError !Name !Dimensions !Point
  deriving (Show, Ord, Eq)
instance Exception DimensionError

data LookupFailure
  = UnmatchedDimensions DimensionError
  | NotFound
  deriving (Show, Ord, Eq)

lookup :: Dimensional a -> Point -> IO (Either LookupFailure a)
lookup d pt
  | not $ matchDimensions (_dimensions d) pt = pure $ Left (UnmatchedDimensions err)
  | otherwise = do
        toLookupResult . HashMap.lookup pt <$> readIORef (_points d)
  where
    err :: DimensionError
    err = DimensionError (_name d) (_dimensions d) pt

    toLookupResult Nothing = Left NotFound
    toLookupResult (Just x) = Right x

matchDimensions :: Dimensions -> Point -> Bool
matchDimensions ds ps = length ds == length ps

create :: Dimensional a -> Point -> IO a
create d pt
  | not $ matchDimensions (_dimensions d) pt = throwIO err
  | otherwise = do
      v <- _create d
      atomicModifyIORef' (_points d) (\store -> (HashMap.insert pt v store, ()))
      return v
  where
    err :: DimensionError
    err = DimensionError (_name d) (_dimensions d) pt

lookupOrCreate :: Dimensional a -> Point -> IO a
lookupOrCreate d pt = lookup d pt >>= \case
    Left NotFound -> create d pt
    Left (UnmatchedDimensions err) -> throwIO err
    Right x -> return x

type Counter = Dimensional Counter.Counter
type Gauge = Dimensional Gauge.Gauge
type Label = Dimensional Label.Label
type Distribution = Dimensional Distribution.Distribution

example :: IO ()
example = do
  c <- newDimensional Counter.new "foo" "a foo" ["url", "status"]
  let url = "/hello"
  let status = "200"
  x <- lookupOrCreate c [url, status]
  Counter.inc x
