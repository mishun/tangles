{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Enumeration.Applied.Test
    ( test
    ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Invariants
import qualified Math.Topology.KnotTh.Moves.AdHoc as AdHoc
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import TestUtil.Table


testInvariantness :: (Eq a, Show a) => ((forall m. (Monad m) => (TangleDiagram -> m ()) -> m ()) -> [AllDiagramsInfo TangleDiagram]) -> Int -> (TangleDiagram -> a) -> Assertion
testInvariantness sortClasses maxN f =
    let sifted = map allDiagrams $ sortClasses $ \ yield -> --tangleDiagrams (-1) n
                    forCCP_ (primeIrreducibleDiagramsTriangle maxN) $ \ (tangle, _) ->
                        yield tangle
    in forM_ sifted $ \ (repr : rest) -> do
            let invariant = f repr
            forM_ rest $ \ cand ->
                invariant @?= f cand


tangleClasses, weakTangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (TangleDiagram -> m ()) -> m ()) -> [info TangleDiagram]
tangleClasses =
    equivalenceClasses
        (map (map reidemeisterReduction .)
            [ reidemeisterIII
            , AdHoc.flype
            , AdHoc.pass
            -- , AdHoc.weak
            ])
weakTangleClasses =
    equivalenceClasses
        (map (map reidemeisterReduction .)
            [ reidemeisterIII
            , AdHoc.flype
            , AdHoc.pass
            , AdHoc.weak
            ])


test :: Test
test = testGroup "Enumeration tests"
    [ testGroup "Invariantness checking for computed classes of tangles"
        [ testCase "Linking numbers" $
            testInvariantness tangleClasses 6 linkingNumbersInvariant

        , testCase "Jones polynomial" $
            testInvariantness tangleClasses 6 minimalKauffmanXPolynomial

        , testCase "Kauffman F polynomial" $
            testInvariantness tangleClasses 5 minimalKauffmanFPolynomial

        , testCase "Jones polynomial of gluing with mirror image (weak classes invariant)" $
            testInvariantness weakTangleClasses 5 (kauffmanXPolynomial . tangleDoubling)

        , testCase "Jones polynomial of doubling satellite" $
            testInvariantness tangleClasses 4 (minimalKauffmanXPolynomial . cablingSatellite 2)

        -- , testCase "Kauffman F polynomial of triple satellite" $
        --    testInvariantness tangleClasses 1 (minimalKauffmanFPolynomial . twistedSatellite 3)
        ]

    , testCase "Enumeration of tangles" $
        testTable'
            (\ maxN -> do
                let sifted =
                        let invariantSet tangle = ( minimalKauffmanXPolynomial tangle
                                                  , minimalKauffmanXPolynomial $ cablingSatellite 2 tangle
                                                  )
                        in siftByInvariant invariantSet $
                            tangleClasses $ \ yield ->
                                forCCP_ (primeIrreducibleDiagramsTriangle maxN) $ \ (tangle, _) ->
                                    yield tangle

                assertEqual "There must be no collisions" 0 $ length $ collisionClasses sifted
                return $! generateTable'
                    (numberOfVertices &&& numberOfLegs)
                    (const 1)
                    (forM_ $ mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted)
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
