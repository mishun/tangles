module Math.Combinatorics.Strings.Test
    ( tests
    ) where

import Test.HUnit
import Math.Combinatorics.Strings.Lyndon


tests :: Test
tests = "String combinatorial functions tests" ~:
    [ "Test minimal cyclic shift" ~: do
        let naive [] = []
            naive l = minimum [ let (a, b) = splitAt i l in b ++ a | i <- [0 .. length l]]

        let list = [6, 4, 3, 4, 5, 3, 3, 1] :: [Int]
        snd (minimumCyclicShift list) @?= naive list
    ]
