{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic increments using 100 concurrent writers.
module Main where

import Control.Concurrent
import Control.Monad
import System.Metrics.Counter

main :: IO ()
main = do
    counter <- new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work counter iters) locks
    mapM_ takeMVar locks
  where
    n = 100
    iters = 100000

    work :: Counter -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work counter i lock = inc counter >> work counter (i - 1) lock
