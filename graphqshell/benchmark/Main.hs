module Main (main) where
import Relude
import Gauge.Main



main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
