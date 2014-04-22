{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
module System.Metrics.ThreadId
    ( myThreadId
    ) where

import qualified Control.Concurrent as Concurrent
import Data.Word (Word32)
import GHC.Conc (ThreadId(..))
import GHC.Prim (ThreadId#)

myThreadId :: IO Int
myThreadId = toInt `fmap` Concurrent.myThreadId
  where
    toInt (ThreadId t) = fromIntegral (getThreadId t)

foreign import ccall unsafe "rts_getThreadId" getThreadId
    :: ThreadId# -> Word32
