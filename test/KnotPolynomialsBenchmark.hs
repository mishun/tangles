module Main (main) where

import Data.Monoid (Last(..))
import Text.Printf
import Criterion.Main
import Criterion.Config
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.Satellites
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.KauffmanFPolynomial


benchmarks :: [Benchmark]
benchmarks =
    [   let ratCode = [2, 3, 4, 5, 6, 7, 8, 20, 40]
            ratTangle = rationalTangle ratCode
        in bgroup (printf "Rational tangle %s" $ show ratCode)
            [ bench "Kauffman F" $
                nf (show . kauffmanFPolynomial) ratTangle

            , bench "Jones" $
                nf (show . jonesPolynomial) ratTangle
            ]

    , bgroup "Twisted triple of loner" $
        let tangle = twistedTripleSatellite lonerOverCrossingTangle
        in  [ bench "Kauffman F" $
                nf (show . kauffmanFPolynomial) tangle

            , bench "Jones" $
                nf (show . jonesPolynomial) tangle
            ]

    , bgroup "Twisted triple of pair" $
        let tangle = twistedTripleSatellite $ rationalTangle [2]
        in  [ bench "Kauffman F" $
                nf (show . kauffmanFPolynomial) tangle

            , bench "Jones" $
                nf (show . jonesPolynomial) tangle
            ]
    ]


main :: IO ()
main =
    defaultMainWith
        (defaultConfig { cfgSamples = Last (Just 1) })
        (return ())
        benchmarks
