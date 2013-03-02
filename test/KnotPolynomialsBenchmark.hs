module Main (main) where

import Data.Monoid (Last(..))
import Text.Printf
import Criterion.Main
import Criterion.Config
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.Satellites
import Math.KnotTh.Link.GaussCode
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.KauffmanFPolynomial


--polynomialsBenchmark :: (SkeinStructure k c d) => k ArbitraryCrossing -> [Benchmark]
polynomialsBenchmark knot =
    [ bench "Kauffman F" $
        nf (show . kauffmanFPolynomial) knot

    , bench "Jones" $
        nf (show . jonesPolynomial) knot
    ]


benchmarks :: [Benchmark]
benchmarks =
    [   let ratCode = [2, 3, 4, 5, 6, 7, 8, 10, 20]
        in bgroup (printf "Big rational tangle %s" $ show ratCode) $
            polynomialsBenchmark $ rationalTangle ratCode

    , bgroup "Twisted triple of loner" $
        let tangle = twistedTripleSatellite lonerOverCrossingTangle
        in polynomialsBenchmark tangle

    , bgroup "Twisted triple of pair" $
        let tangle = twistedTripleSatellite $ rationalTangle [2]
        in polynomialsBenchmark tangle

    , bgroup "Tricky link with 7 threads" $
        let link = fromGaussCode
                [ [1,  -8, 15, -22, 29, -36, 37, -31, 25, -19, 13, -7]
                , [2,  -9, 16, -23, 30, -37, 38, -32, 26, -20, 14, -1]
                , [3, -10, 17, -24, 31, -38, 39, -33, 27, -21,  8, -2]
                , [4, -11, 18, -25, 32, -39, 40, -34, 28, -15,  9, -3]
                , [5, -12, 19, -26, 33, -40, 41, -35, 22, -16, 10, -4]
                , [6, -13, 20, -27, 34, -41, 42, -29, 23, -17, 11, -5]
                , [7, -14, 21, -28, 35, -42, 36, -30, 24, -18, 12, -6]
                ]
        in polynomialsBenchmark link

    , bgroup "Tricky link with 9 threads" $
        let link = fromGaussCode
                [ [1, -10, 19, -28, 37, -46, 55, -64, 65, -57, 49, -41, 32, -25, 17, -9]
                , [2, -11, 20, -29, 38, -47, 56, -65, 66, -58, 50, -42, 34, -26, 18, -1]
                , [3, -12, 21, -30, 39, -48, 57, -66, 67, -59, 51, -43, 35, -27, 10, -2]
                , [4, -13, 22, -31, 40, -49, 58, -67, 68, -60, 52, -44, 36, -19, 11, -3]
                , [5, -14, 23, -32, 41, -50, 59, -68, 69, -61, 53, -45, 28, -20, 12, -4]
                , [6, -15, 24, -33, 42, -51, 60, -69, 70, -62, 54, -37, 29, -21, 13, -5]
                , [7, -16, 25, -34, 43, -52, 61, -70, 71, -63, 46, -38, 30, -22, 14, -6]
                , [8, -17, 26, -35, 44, -53, 62, -71, 72, -55, 47, -39, 31, -23, 15, -7]
                , [9, -18, 27, -36, 45, -54, 63, -72, 64, -56, 48, -40, 32, -24, 16, -8]
                ]
        in polynomialsBenchmark link
    ]


main :: IO ()
main =
    defaultMainWith
        (defaultConfig { cfgSamples = Last (Just 1) })
        (return ())
        benchmarks
