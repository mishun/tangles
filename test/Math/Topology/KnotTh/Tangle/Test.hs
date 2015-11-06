module Math.Topology.KnotTh.Tangle.Test
    ( test
    ) where

import Control.Monad
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Math.Topology.KnotTh.Tangle


test :: Test
test = testGroup "Basic tangle tests"
    [ testCase "Very basic functions" $ do
        let t = vertexOwner $ glueToBorder 1 (lonerProjection, 0) ProjectionCrossing
        let c1 = nthVertex t 1
        vertexIndex c1 @?= 1
        opposite (nthLeg t 3) @?= nthOutcomingDart c1 1

        forM_ [0 .. 3] $ \ i -> do
            nextCW (nthOutcomingDart c1 i) @?= nthOutcomingDart c1 ((i - 1) `mod` 4)
            nextCCW (nthOutcomingDart c1 i) @?= nthOutcomingDart c1 ((i + 1) `mod` 4)

        forM_ [0 .. 5] $ \ i -> do
            nextCW (nthLeg t i) @?= nthLeg t ((i - 1) `mod` 6)
            nextCCW (nthLeg t i) @?= nthLeg t ((i + 1) `mod` 6)

        foldMOutcomingDartsFrom (nthOutcomingDart c1 2) ccw (\ _ s -> return $! s + 1) (0 :: Int) >>= (@?= 4)

    , testCase "Show tangle" $ do
        assertEqual "empty tangle" "implode (0,[],[])" $
            show (toTangle (unlink 0))

        assertEqual "zero tangle" "implode (0,[(0,3),(0,2),(0,1),(0,0)],[])" $
            show (toTangle zeroTangle :: TangleProjection)

        assertEqual "infinity tangle" "implode (0,[(0,1),(0,0),(0,3),(0,2)],[])" $
            show (toTangle infinityTangle :: TangleProjection)

        assertEqual "loner tangle" "implode (0,[(1,0),(1,1),(1,2),(1,3)],[([(0,0),(0,1),(0,2),(0,3)],ProjectionCrossing)])" $
            show (toTangle lonerProjection)

        assertEqual "implode" "implode (0,[(1,0),(1,1),(1,2),(1,3)],[([(0,0),(0,1),(0,2),(0,3)],ProjectionCrossing)])" $
            show (implode (0, [(1, 0), (1, 1), (1, 2), (1, 3)], [([(0, 0), (0, 1), (0, 2), (0, 3)], ProjectionCrossing)]) :: TangleProjection)

    , testCase "Cascade code" $
        explode (decodeCascadeCodeFromPairs [(1, 0), (0, 5), (0, 3), (0, 3), (0, 5)]) @?=
            ( 0
            , [(6, 2), (6, 3), (5, 3), (2, 3), (4, 2), (4, 3)]
            ,   [ ([(2, 0), (4, 1), (4, 0), (6, 1)], ProjectionCrossing)
                , ([(1, 0), (3, 1), (3, 0), (0, 3)], ProjectionCrossing)
                , ([(2, 2), (2, 1), (5, 1), (5, 0)], ProjectionCrossing)
                , ([(1, 2), (1, 1), (0, 4), (0, 5)], ProjectionCrossing)
                , ([(3, 3), (3, 2), (6, 0), (0, 2)], ProjectionCrossing)
                , ([(5, 2), (1, 3), (0, 0), (0, 1)], ProjectionCrossing)
                ]
            )

    , testCase "Alternating defect" $ do
        totalAlternatingDefect (decodeCascadeCode [(WU, 0), (MO, 0)]) @?= 0
        totalAlternatingDefect (decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)]) @?= 2

    , testGroup "Glue crossing"
        [ testCase "With 0 legs" $
            explode (vertexOwner $ glueToBorder 0 (lonerProjection, 0) ProjectionCrossing) @?=
                ( 0
                , [(2, 0), (2, 1), (2, 2), (2, 3), (1, 1), (1, 2), (1, 3), (1, 0)]
                ,   [ ([(0, 7), (0, 4), (0, 5), (0, 6)], ProjectionCrossing)
                    , ([(0, 0), (0, 1), (0, 2), (0, 3)], ProjectionCrossing)
                    ]
                )

        , testCase "With 1 leg" $
            explode (vertexOwner $ glueToBorder 1 (lonerProjection, 0) ProjectionCrossing) @?=
                ( 0
                , [(2, 1), (2, 2), (2, 3), (1, 1), (1, 2), (1, 3)]
                ,   [ ([(2, 0), (0, 3), (0, 4), (0, 5)], ProjectionCrossing)
                    , ([(1, 0), (0, 0), (0, 1), (0, 2)], ProjectionCrossing)
                    ]
                )

        , testCase "With 2 legs" $
            explode (vertexOwner $ glueToBorder 2 (lonerProjection, 1) ProjectionCrossing) @?=
                ( 0
                , [(2, 2), (2, 3), (1, 2), (1, 3)]
                ,   [ ([(2, 1), (2, 0), (0, 2), (0, 3)], ProjectionCrossing)
                    , ([(1, 1), (1, 0), (0, 0), (0, 1)], ProjectionCrossing)
                    ]
                )

        , testCase "with 3 legs" $
            explode (vertexOwner $ glueToBorder 3 (lonerProjection, 3) ProjectionCrossing) @?=
                ( 0
                , [(2, 3), (1, 0)]
                ,   [ ([(0, 1), (2, 2), (2, 1), (2, 0)], ProjectionCrossing)
                    , ([(1, 3), (1, 2), (1, 1), (0, 0)], ProjectionCrossing)
                    ]
                )

        , testCase "With 4 legs" $
            explode (vertexOwner $ glueToBorder 4 (lonerProjection, 1) ProjectionCrossing) @?=
                ( 0
                , []
                ,   [ ([(2, 1), (2, 0), (2, 3), (2, 2)], ProjectionCrossing)
                    , ([(1, 1), (1, 0), (1, 3), (1, 2)], ProjectionCrossing)
                    ]
                )
        ]

    , testGroup "Glue tangles"
        [ testCase "Glue 2 loner tangles" $
            let t = toTangle lonerProjection
            in explode (horizontalComposition 1 (t, 0) (t, 1)) @?=
                ( 0
                , [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (2, 0)]
                ,   [ ([(2, 1), (0, 0), (0, 1), (0, 2)], ProjectionCrossing)
                    , ([(0, 5), (1, 0), (0, 3), (0, 4)], ProjectionCrossing)
                    ]
                )

        , testCase "Glue zero and infinity tangles to infinity" $
            let z = toTangle zeroTangle :: TangleProjection
                i = toTangle infinityTangle :: TangleProjection
            in explode (horizontalComposition 2 (z, 0) (z, 3)) @?= explode i

        , testCase "Glue two infinity tangles to get circle inside" $
            let i = toTangle infinityTangle :: TangleProjection
            in explode (horizontalComposition 2 (i, 0) (i, 2)) @?= (1, [(0, 1), (0, 0), (0, 3), (0, 2)], [])

        , testCase "Glue loner and thread" $
            explode (horizontalComposition 2 (toTangle lonerProjection, 0) (planarPropagator 1, 0)) @?=
                ( 0
                , [(1, 2), (1, 3)]
                ,   [ ([(1, 1), (1, 0), (0, 0), (0, 1)], ProjectionCrossing)
                    ]
                )
        ]

    , testGroup "Braid tangles"
        [ testCase "Identity braid tangle" $
            explode (toTangle (identityBraid 4) :: TangleProjection) @?=
                (0, [(0, 7), (0, 6), (0, 5), (0, 4), (0, 3), (0, 2), (0, 1), (0, 0)], [])

        , testCase "Braid generator" $
            explode (toTangle (braid 3 [(1, OverCrossing)])) @?=
                (0, [(0, 5), (1, 0), (1, 1), (1, 2), (1, 3), (0, 0)], [([(0, 1), (0, 2), (0, 3), (0, 4)], OverCrossing)])

        , testCase "Braid tangle" $
            explode (toTangle (braid 3 [(0, OverCrossing), (1, OverCrossing), (0, OverCrossing)])) @?=
                ( 0
                , [(1, 0), (1, 1), (2, 1), (2, 2), (3, 2), (3, 3)]
                ,   [ ([(0, 0), (0, 1), (2, 0), (3, 0)], OverCrossing)
                    , ([(1, 2), (0, 2), (0, 3), (3, 1)], OverCrossing)
                    , ([(1, 3), (2, 3), (0, 4), (0, 5)], OverCrossing)
                    ]
                )
        ]

    , testGroup "Rational tangles"
        [ testProperty "Always alternating" $ \ list ->
            all (\ x -> x > 0 && x < 1000) list ==> isAlternating $ toTangle $ rationalTangle list

        , testProperty "Conway reciprocate" $ \ list ->
            let t = rationalTangle list
                f = unrootedHomeomorphismInvariant . toTangle
            in all ((< 12) . abs) list ==> f (conwayRecip t) == f (conwayProduct t zeroTangle)

        , testCase "Numerator closure" $ do
            numberOfFreeLoops (numeratorClosure zeroTangle :: Tangle0 DiagramCrossing) @?= 2
        ]

    , testGroup "Homeomorphism invariant" $
        [ testCase "unlinks" $ do
            assertBool "0 & 1" $ unrootedHomeomorphismInvariant (unlink 0) /= unrootedHomeomorphismInvariant (unlink 1)
            assertBool "1 & 2" $ unrootedHomeomorphismInvariant (unlink 1) /= unrootedHomeomorphismInvariant (unlink 2)

        , testCase "0 & ∞" $
            unrootedHomeomorphismInvariant (zeroTangle :: Tangle4 DiagramCrossing)
                @?= unrootedHomeomorphismInvariant (infinityTangle :: Tangle4 DiagramCrossing)
        ]
    ]
