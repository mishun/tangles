{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Tangle.BorderIncremental.Test
    ( tests
    ) where

import Control.Monad (when)
import Test.HUnit
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator
import TestUtil.Table


tests :: Test
tests = "Tangle generators" ~:
    [ "Numbers of prime tangle projections" ~:
        testTable (\ n -> generateTable False $ simpleIncrementalGenerator primeProjectionType [ProjectionCrossing] n)
            [ [1]
            , [1, 1]
            , [2, 2, 2]
            , [6, 8, 8, 5]
            , [19, 29, 41, 31, 16]
            , [71, 138, 210, 231, 161, 60]
            , [293, 638, 1125, 1458, 1406, 840, 261]
            , [1348, 3237, 6138, 9183, 10572, 8818, 4702, 1243]
            ]

    , "Numbers of basic polyhedral tangle projections" ~:
        testTable (\ n -> generateTable False $ simpleIncrementalGenerator reducedProjectionType [ProjectionCrossing] n)
            [ [1]
            , [0, 1]
            , [0, 1, 2]
            , [0, 1, 2, 5]
            , [1, 1, 4, 9, 16]
            , [1, 4, 7, 22, 42, 60]
            , [3, 7, 21, 49, 126, 228, 261]
            ]

    , "Numbers of tangle templates" ~:
        testTable (\ n -> generateTable False $ simpleIncrementalGenerator templateProjectionType [ProjectionCrossing] n)
            [ [1]
            , [0, 1]
            , [0, 1, 2]
            , [0, 1, 2, 5]
            , [1, 1, 4, 9, 16]
            , [0, 3, 7, 22, 42, 60]
            , [1, 4, 17, 49, 126, 228, 261]
            , [2, 12, 43, 139, 355, 799, 1288, 1243]
            ]

    , "Numbers of tangle diagrams" ~:
        testTable (\ n -> generateTable False $ simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] n)
            [ [1]
            , [1, 3] -- [1, 2]
            , [3, 6, 10] -- [3, 4, 6]
            , [18, 41, 58, 58] -- [14, 25, 33, 32]
            , [116, 268, 484, 564, 397] -- [76, 148, 258, 290, 206]
            , [836, 2168, 4120, 6070, 6099, 3388] -- [486, 1146, 2125, 3086, 3081, 1718]
            ]

    , "Numbers of alternating tangles" ~:
        testTable (\ n -> generateTable False $ generateFlypeEquivalent n)
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

    , "Numbers of 4-leg alternating tangles without symmetry" ~:
        testTable
            (\ n -> generateTable True $ \ yield -> generateFlypeEquivalentInTriangle n (\ t s -> when (numberOfLegs t == 4) $ yield t s))
            [[1], [2], [4], [10], [29], [98], [372], [1538], [6755], [30996]]
    ]
