{-# LANGUAGE OverloadedStrings #-}
import qualified Data.HashMap.Strict as M
import GHC.Stats
import System.Metrics

main = do
    store <- newStore
    let metrics =
            [ ("num_gcs", Counter . numGcs)
            , ("max_bytes_used", Gauge . maxBytesUsed)
            ]
    registerGroup (M.fromList metrics) getGCStats store
