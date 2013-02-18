module Math.KnotTh.Tangle.Test
    ( test
    ) where

import Control.Monad
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Algebra.RotationDirection
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.CascadeCode


test :: Test
test = testGroup "Basic tangle tests"
    [ testCase "Very basic functions" $ do
        let t = crossingTangle $ glueToBorder (firstLeg lonerProjection) 1 projectionCrossing
        let c1 = nthCrossing t 1
        crossingIndex c1 @?= 1
        opposite (nthLeg t 3) @?= nthIncidentDart c1 1

        forM_ [0 .. 3] $ \ i -> do
            nextCW (nthIncidentDart c1 i) @?= nthIncidentDart c1 ((i - 1) `mod` 4)
            nextCCW (nthIncidentDart c1 i) @?= nthIncidentDart c1 ((i + 1) `mod` 4)

        forM_ [0 .. 5] $ \ i -> do
            nextCW (nthLeg t i) @?= nthLeg t ((i - 1) `mod` 6)
            nextCCW (nthLeg t i) @?= nthLeg t ((i + 1) `mod` 6)

        foldMIncidentDartsFrom (nthIncidentDart c1 2) ccw (\ _ s -> return $! s + 1) (0 :: Int) >>= (@?= 4)

    , testCase "Show tangle" $ do
        assertEqual "empty tangle" "(Tangle (0 O) (Border [  ]))" $
            show (emptyTangle :: TangleProjection)

        assertEqual "zero tangle" "(Tangle (0 O) (Border [ (Leg 3) (Leg 2) (Leg 1) (Leg 0) ]))" $
            show (zeroTangle :: TangleProjection)

        assertEqual "infinity tangle" "(Tangle (0 O) (Border [ (Leg 1) (Leg 0) (Leg 3) (Leg 2) ]))" $
            show (infinityTangle :: TangleProjection)

        assertEqual "loner tangle" "(Tangle (0 O) (Border [ (Dart 1 0) (Dart 1 1) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Leg 0) (Leg 1) (Leg 2) (Leg 3) ]))" $
            show lonerProjection

        assertEqual "implode" "(Tangle (0 O) (Border [ (Dart 1 0) (Dart 1 1) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Leg 0) (Leg 1) (Leg 2) (Leg 3) ]))" $
            show (implode (0, [(1, 0), (1, 1), (1, 2), (1, 3)], [([(0, 0), (0, 1), (0, 2), (0, 3)], projectionCrossing)]) :: TangleProjection)

    , testCase "Cascade code" $
        explode (decodeCascadeCodeFromPairs [(1, 0), (0, 5), (0, 3), (0, 3), (0, 5)]) @?=
            ( 0
            , [(6, 2), (6, 3), (5, 3), (2, 3), (4, 2), (4, 3)]
            ,   [ ([(2, 0), (4, 1), (4, 0), (6, 1)], projectionCrossing)
                , ([(1, 0), (3, 1), (3, 0), (0, 3)], projectionCrossing)
                , ([(2, 2), (2, 1), (5, 1), (5, 0)], projectionCrossing)
                , ([(1, 2), (1, 1), (0, 4), (0, 5)], projectionCrossing)
                , ([(3, 3), (3, 2), (6, 0), (0, 2)], projectionCrossing)
                , ([(5, 2), (1, 3), (0, 0), (0, 1)], projectionCrossing)
                ]
            )

    , testGroup "Glue crossing"
        [ testCase "With 0 legs" $
            explode (crossingTangle $ glueToBorder (nthLeg lonerProjection 0) 0 projectionCrossing) @?=
                ( 0
                , [(2, 0), (2, 1), (2, 2), (2, 3), (1, 1), (1, 2), (1, 3), (1, 0)]
                ,   [ ([(0, 7), (0, 4), (0, 5), (0, 6)], projectionCrossing)
                    , ([(0, 0), (0, 1), (0, 2), (0, 3)], projectionCrossing)
                    ]
                )

        , testCase "With 1 leg" $
            explode (crossingTangle $ glueToBorder (firstLeg lonerProjection) 1 projectionCrossing) @?=
                ( 0
                , [(2, 1), (2, 2), (2, 3), (1, 1), (1, 2), (1, 3)]
                ,   [ ([(2, 0), (0, 3), (0, 4), (0, 5)], projectionCrossing)
                    , ([(1, 0), (0, 0), (0, 1), (0, 2)], projectionCrossing)
                    ]
                )

        , testCase "With 2 legs" $
            explode (crossingTangle $ glueToBorder (nthLeg lonerProjection 1) 2 projectionCrossing) @?=
                ( 0
                , [(2, 2), (2, 3), (1, 2), (1, 3)]
                ,   [ ([(2, 1), (2, 0), (0, 2), (0, 3)], projectionCrossing)
                    , ([(1, 1), (1, 0), (0, 0), (0, 1)], projectionCrossing)
                    ]
                )

        , testCase "with 3 legs" $
            explode (crossingTangle $ glueToBorder (nthLeg lonerProjection 3) 3 projectionCrossing) @?=
                ( 0
                , [(2, 3), (1, 0)]
                ,   [ ([(0, 1), (2, 2), (2, 1), (2, 0)], projectionCrossing)
                    , ([(1, 3), (1, 2), (1, 1), (0, 0)], projectionCrossing)
                    ]
                )

        , testCase "With 4 legs" $
            explode (crossingTangle $ glueToBorder (nthLeg lonerProjection 1) 4 projectionCrossing) @?=
                ( 0
                , []
                ,   [ ([(2, 1), (2, 0), (2, 3), (2, 2)], projectionCrossing)
                    , ([(1, 1), (1, 0), (1, 3), (1, 2)], projectionCrossing)
                    ]
                )
        ]

    , testGroup "Glue tangles"
        [ testCase "Glue 2 loner tangles" $
            let t = lonerProjection
            in explode (glueTangles 1 (nthLeg t 0) (nthLeg t 1)) @?=
                ( 0
                , [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (2, 0)]
                ,   [ ([(2, 1), (0, 0), (0, 1), (0, 2)], projectionCrossing)
                    , ([(0, 5), (1, 0), (0, 3), (0, 4)], projectionCrossing)
                    ]
                )

        , testCase "Glue zero and infinity tangles to infinity" $
            let z = zeroTangle :: TangleProjection
                i = infinityTangle :: TangleProjection
            in explode (glueTangles 2 (nthLeg z 0) (nthLeg z 0)) @?= explode i

        , testCase "Glue two infinity tangles to get circle inside" $
            let i = infinityTangle :: TangleProjection
            in explode (glueTangles 2 (nthLeg i 0) (nthLeg i 3)) @?= (1, [(0, 1), (0, 0), (0, 3), (0, 2)], [])

        , testCase "Glue loner and thread" $
            explode (glueTangles 2 (firstLeg lonerProjection) (firstLeg identityTangle)) @?=
                ( 0
                , [(1, 2), (1, 3)]
                ,   [ ([(1, 1), (1, 0), (0, 0), (0, 1)], projectionCrossing)
                    ]
                )
        ]

    , testGroup "Braid tangles"
        [ testCase "Identity braid tangle" $
            explode (identityBraidTangle 4 :: TangleProjection) @?=
                (0, [(0, 7), (0, 6), (0, 5), (0, 4), (0, 3), (0, 2), (0, 1), (0, 0)], [])

        , testCase "Braid generator" $
            explode (braidGeneratorTangle 3 (1, overCrossing)) @?=
                (0, [(0, 5), (1, 0), (1, 1), (1, 2), (1, 3), (0, 0)], [([(0, 1), (0, 2), (0, 3), (0, 4)], overCrossing)])

        , testCase "Braid tangle" $
            explode (braidTangle 3 [(0, overCrossing), (1, overCrossing), (0, overCrossing)]) @?=
                ( 0
                , [(1, 0), (1, 1), (2, 1), (2, 2), (3, 2), (3, 3)]
                ,   [ ([(0, 0), (0, 1), (2, 0), (3, 0)], overCrossing)
                    , ([(1, 2), (0, 2), (0, 3), (3, 1)], overCrossing)
                    , ([(1, 3), (2, 3), (0, 4), (0, 5)], overCrossing)
                    ]
                )
        ]
    ]
