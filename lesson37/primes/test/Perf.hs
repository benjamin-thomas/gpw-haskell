{-# LANGUAGE NumericUnderscores #-}

module Perf (main) where

import Criterion (Benchmark, bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import Primes (sieve1, sieve4, sieve4')

benchSieve :: String -> ([Int] -> [Int]) -> Benchmark
benchSieve name sieveFunc =
    bgroup
        name
        [ bench "sum [2..10000]" $ whnf (sum . sieveFunc) [2 .. 10_000]
        , bench "take 3 from [2..10000000]" $ whnf (take 3 . sieveFunc) [2 .. 10_000_000]
        ]

main :: IO ()
main =
    defaultMain
        [ benchSieve "sieve1" sieve1
        , benchSieve "sieve4" sieve4
        , benchSieve "sieve4'" sieve4'
        ]
