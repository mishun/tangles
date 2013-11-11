{-# LANGUAGE Rank2Types #-}
module Math.Topology.KnotTh.Tangle.Generation.Test
    ( test
    ) where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
import Math.Topology.KnotTh.Tangle.Generation.FlypeClasses
import TestUtil.Table


test :: Test
test = testGroup "Tangle generators"
    [ testCase "Numbers of prime tangle projections" $
        testTable (\ n -> generateTable $ forCCP_ (primeProjections n))
            [ [1]
            , [1, 1]
            , [2, 2, 2]
            , [6, 8, 8, 5]
            , [19, 29, 41, 31, 16]
            , [71, 138, 210, 231, 161, 60]
            , [293, 638, 1125, 1458, 1406, 840, 261]
            , [1348, 3237, 6138, 9183, 10572, 8818, 4702, 1243]
            ]

    , testCase "Numbers of basic polyhedral tangle projections" $
        testTable (\ n -> generateTable $ forCCP_ (reducedProjections n))
            [ [1]
            , [0, 1]
            , [0, 1, 2]
            , [0, 1, 2, 5]
            , [1, 1, 4, 9, 16]
            , [1, 4, 7, 22, 42, 60]
            , [3, 7, 21, 49, 126, 228, 261]
            ]

    , testCase "Numbers of tangle templates" $
        testTable (\ n -> generateTable $ forCCP_ (templateProjections n))
            [ [1]
            , [0, 1]
            , [0, 1, 2]
            , [0, 1, 2, 5]
            , [1, 1, 4, 9, 16]
            , [0, 3, 7, 22, 42, 60]
            , [1, 4, 17, 49, 126, 228, 261]
            , [2, 12, 43, 139, 355, 799, 1288, 1243]
            ]

    , testCase "Numbers of tangle diagrams" $
        testTable (\ n -> generateTable $ forCCP_ (primeIrreducibleDiagrams n))
            [ [1]
            , [1, 2]
            , [3, 4, 6]
            , [14, 25, 33, 32]
            , [76, 148, 258, 290, 206]
            , [486, 1146, 2125, 3086, 3081, 1718]
            ]

    , testCase "Numbers of alternating tangles" $
        testTable (\ n -> generateTable $ generateFlypeEquivalent n)
            [ [1]
            , [1, 1]
            , [2, 2, 2]
            , [5, 7, 8, 5]
            , [13, 20, 37, 31, 16]
            , [36, 77, 157, 209, 161, 60]
            , [111, 276, 687, 1128, 1294, 840, 261]
            , [373, 1135, 3052, 5986, 8528, 8206, 4702, 1243]
            , [1362, 4823, 13981, 30556, 51475, 62895, 52815, 26753, 6257]
            ]

    , testCase "Numbers of 4-leg alternating tangles without symmetry" $
        testTable
            (\ n -> generateTable'
                ((numberOfCrossings &&& numberOfLegs) . fst)
                (\ (_, symmetry) -> Dn.rotationPeriod symmetry * (if Dn.hasReflectionPart symmetry then 1 else 2))
                (\ yield -> generateFlypeEquivalentInTriangle n
                    (\ (t, s) -> when (numberOfLegs t == 4) $
                        yield (t, s)
                    )
                )
            )
            [[1], [2], [4], [10], [29], [98], [372], [1538], [6755], [30996]]
    ]
