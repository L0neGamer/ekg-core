{-# LANGUAGE BangPatterns
           , CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           #-}
-- | An atomic integer value. All operations are thread safe.
module Data.Atomic
    (
      Atomic
    , new
    , read
    , write
    , inc
    , dec
    , add
    , subtract
    ) where

import Prelude hiding (read, subtract)

import GHC.Int
import GHC.IO
import GHC.Prim

#include "MachDeps.h"

#if WORD_SIZE_IN_BYTES > 32
#define ARRLEN 8
#else
#define ARRLEN 4
#endif

-- | A mutable, atomic integer.
--newtype Atomic = C (ForeignPtr Int64)
data Atomic = C (MutableByteArray# RealWorld)

-- | Create a new, zero initialized, atomic.
new :: Int -> IO Atomic
new (I# n) = IO $ \s ->
    case newByteArray# ARRLEN# s of { (# s1, mba #) ->
    case atomicWriteIntArray# mba 0# n s1 of { s2 ->
    (# s2, C mba #) }}

read :: Atomic -> IO Int
read (C mba) = IO $ \s ->
    case atomicReadIntArray# mba 0# s of { (# s1, n #) ->
    (# s1, I# n #)}

-- | Set the atomic to the given value.
write :: Atomic -> Int -> IO ()
write (C mba) (I# n) = IO $ \s ->
    case atomicWriteIntArray# mba 0# n s of { s1 ->
    (# s1, () #) }

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

-- | Decrease the atomic by one.
dec :: Atomic -> IO ()
dec atomic = subtract atomic 1

-- | Increase the atomic by the given amount.
add :: Atomic -> Int -> IO ()
add (C mba) (I# n) = IO $ \s ->
    case fetchAddIntArray# mba 0# n s of { (# s1, _ #) ->
    (# s1, () #) }

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int -> IO ()
subtract (C mba) (I# n) = IO $ \s ->
    case fetchSubIntArray# mba 0# n s of { (# s1, _ #) ->
    (# s1, () #) }
