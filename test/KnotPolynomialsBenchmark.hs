module Main (main) where

import Text.Printf
import Criterion.Main
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Invariants.KauffmanFPolynomial


main :: IO ()
main = defaultMain
    [   let rat = [2, 3, 4, 5, 6, 7, 8, 20]
        in bench (printf "Kauffman F on rational tangle %s" $ show rat) $
            nf (show . kauffmanFPolynomial) (rationalTangle rat) 
    ]
