import qualified System.Metrics.Counter as Counter
import System.Metrics.Dimensional

main :: IO ()
main = do
  c <-
    newDimensional
      Counter.new
      "wai.response"
      "endpoints status response"
      ["url", "status"]
  let url = "/hello"
  let status = "200"
  x <- lookupOrCreate c [url, status]
  Counter.inc x
