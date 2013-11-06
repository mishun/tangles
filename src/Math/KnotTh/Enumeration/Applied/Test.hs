{-# LANGUAGE RankNTypes #-}
module Math.KnotTh.Enumeration.Applied.Test
    ( test
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import Control.Parallel.Strategies
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.KnotTh.Tangle
import Math.KnotTh.Tangle.NonAlternating.Satellites
import Math.KnotTh.Link (tangleDoubling)
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Invariants
import TestUtil.Table


testInvariantness ::
    (Eq a, Show a)
        => ((forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ())
        -> [AllDiagramsInfo NonAlternatingTangle])
        -> Int
        -> (NonAlternatingTangle -> a)
        -> Assertion

testInvariantness sortClasses n f = do
    let classes = map allDiagrams $ sortClasses $ tangleDiagrams True (-1) n
    let results =
            parMap rdeepseq (\ cls ->
                    let inv = map f cls
                    in all (== head inv) inv
                ) classes
    assert $ and results


test :: Test
test = testGroup "Enumeration tests"
    [ testGroup "Invariantness checking for computed classes of tangles"
        [ testCase "Linking numbers" $
            testInvariantness tangleClasses 6 linkingNumbersInvariant

        , testCase "Jones polynomial" $
            testInvariantness tangleClasses 6 minimalJonesPolynomialOfTangle

        , testCase "Kauffman F polynomial" $
            testInvariantness tangleClasses 5 minimalKauffmanFPolynomialOfTangle

        , testCase "Jones polynomial of gluing with mirror image (weak classes invariant)" $
            testInvariantness weakTangleClasses 5 (jonesPolynomial . tangleDoubling id)

        , testCase "Jones polynomial of doubling satellite" $
            testInvariantness tangleClasses 4 (minimalJonesPolynomialOfTangle . twistedDoubleSatellite)

        , testCase "Kauffman F polynomial of triple satellite" $
            testInvariantness tangleClasses 1 (minimalKauffmanFPolynomialOfTangle . twistedTripleSatellite)
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
