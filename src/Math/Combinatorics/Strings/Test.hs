module Math.Combinatorics.Strings.Test
    ( test
    ) where

import Text.Printf
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Combinatorics.Strings.Lyndon


naiveMinShift :: (Ord a) => [a] -> [a]
naiveMinShift [] = []
naiveMinShift l = minimum [ let (a, b) = splitAt i l in b ++ a | i <- [0 .. length l]]


minShiftIsOk :: [Int] -> Bool
minShiftIsOk list = snd (minimumCyclicShift list) == naiveMinShift list


test :: Test
test = testGroup "String combinatorial functions tests"
    [ testProperty "Minimal cyclic shift" minShiftIsOk

    ,   let list = [6, 4, 3, 4, 5, 3, 3, 1] :: [Int]
        in testCase (printf "Test minimal cyclic shift of %s" $ show list) $
            snd (minimumCyclicShift list) @?= naiveMinShift list
    ]
