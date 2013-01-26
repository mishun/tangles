module Math.KnotTh.Enumeration.Applied.Test
    ( tests
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import Test.HUnit
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.TwistedDouble
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.JonesPolynomial
import TestUtil.Table


testInvariantness :: (Eq a, Show a) => Int -> (NonAlternatingTangle -> a) -> IO ()
testInvariantness n f =
    forM_ (map allDiagrams $ tangleClasses $ tangleDiagrams True (-1) n) $ \ cls -> do
        let inv = map f cls
        mapM_ (@?= head inv) inv


tests :: Test
tests = "Enumeration tests" ~:
    [ "Invariantness checking for computed classes of tangles" ~: 
        [ "Linking numbers" ~:
            testInvariantness 6 linkingNumbersSet

        , "Jones polynomial" ~:
            testInvariantness 6 minimalJonesPolynomialOfTangle

        , "Jones polynomial of doubling" ~:
            testInvariantness 4 (minimalJonesPolynomialOfTangle . twistedDouble)
        ]

    , "Enumeration of tangles" ~:
        testTable'
            (\ n -> do
                let sifted = lookingForwardTanglesEnumeration True (-1) 0 n
                assertEqual "There must be no collisions" 0 $ length $ collisionClasses sifted
                return $! generateTable' $ forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted)
            )
            [ [1]
            , [1, 2]
            , [2, 4, 6]
            , [6, 15, 30, 32]
            , [15, 48, 154]
            , [47, 201]
            , [156]
            ]

{-    , "Enumeration of weak tangles" ~:
        testTable'
            (\ n -> do
                let sifted = lookingForwardWeakTanglesEnumeration True (-1) 2 n
                assertEqual "There must be no collisions" 0 $ length $ collisionClasses sifted
                return $! generateTable' $ forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted)
            )
            [ []
            , []
            , []
            , [1]
            , [2]
            , [8, 1]
            ]-}
    ]
