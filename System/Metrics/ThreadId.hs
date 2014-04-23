{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
module System.Metrics.ThreadId
    ( myCapability
    ) where

import qualified Control.Concurrent as Concurrent

myCapability :: IO Int
myCapability = (return . fst) =<< Concurrent.threadCapability =<<
               Concurrent.myThreadId

