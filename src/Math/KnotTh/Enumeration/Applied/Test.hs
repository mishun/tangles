module Math.KnotTh.Enumeration.Applied.Test
    ( test
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.TwistedDouble
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.JonesPolynomial
import TestUtil.Table


testInvariantness :: (Eq a, Show a) => Int -> (NonAlternatingTangle -> a) -> Assertion
testInvariantness n f =
    forM_ (map allDiagrams $ tangleClasses $ tangleDiagrams True (-1) n) $ \ cls -> do
        let inv = map f cls
        mapM_ (@?= head inv) inv


test :: Test
test = testGroup "Enumeration tests" $
    [ testGroup "Invariantness checking for computed classes of tangles" $
        [ testCase "Linking numbers" $
            testInvariantness 6 linkingNumbersSet

        , testCase "Jones polynomial" $
            testInvariantness 6 minimalJonesPolynomialOfTangle

        , testCase "Jones polynomial of doubling" $
            testInvariantness 4 (minimalJonesPolynomialOfTangle . twistedDouble)
        ]

    , testCase "Enumeration of tangles" $
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

{-    , testCase "Enumeration of weak tangles" $
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
