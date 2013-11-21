module Math.Topology.KnotTh.EmbeddedLink.Test
    ( test
    ) where

import Control.Monad (forM_)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TestPrime


test :: Test
test =
    let testCorr link = do
            forM_ (allHalfEdges link) $ \ d -> do
                let f = leftFace d
                    p = leftPlace d
                nthDartInCCWTraverse f p @?= d

            forM_ (allHalfEdges link) $ \ d -> do
                let f = rightFace d
                    p = rightPlace d
                nthDartInCWTraverse f p @?= d

    in testGroup "EmbeddedLink tests"
        [ testCase "Simplest torus link" $ do
            let l :: EmbeddedLinkProj
                l = implode (0, [([(1, 2), (1, 3), (1, 0), (1, 1)], projectionCrossing)])

            testCorr l
            assertEqual "Number of faces" 1 $ numberOfFaces l
            assertEqual "Face degree" 4 $ faceDegree (nthFace l 1)
            assertEqual "Euler char" 0 $ eulerChar l

        , testCase "Hopf link" $ do
            let l :: EmbeddedLinkProj
                l = implode (0,
                    [ ([(2, 0), (2, 3), (2, 2), (2, 1)], projectionCrossing)
                    , ([(1, 0), (1, 3), (1, 2), (1, 1)], projectionCrossing)
                    ])

            testCorr l
            assertEqual "Number of faces" 4 $ numberOfFaces l
            assertEqual "Euler char" 2 $ eulerChar l

        , testCase "Test prime on one vertex" $
            True @=? testPrime (implode (0, [([(1, 2), (1, 3), (1, 0), (1, 1)], projectionCrossing)]))

        , testCase "Test prime on hopf link" $
            True @=? testPrime (implode (0,
                [ ([(2, 0), (2, 3), (2, 2), (2, 1)], projectionCrossing)
                , ([(1, 0), (1, 3), (1, 2), (1, 1)], projectionCrossing)
                ]))

        , testCase "Test prime on ooo" $
            False @=? testPrime (implode (0,
                [ ([(2, 0), (2, 3), (1, 3), (1, 2)], projectionCrossing)
                , ([(1, 0), (2, 2), (2, 1), (1, 1)], projectionCrossing)
                ]))

        , testCase "Test prime on square knot" $
            False @=? testPrime (implode (0,
                [ ([(2, 0), (2, 3), (4, 0), (3, 1)], projectionCrossing)
                , ([(1, 0), (3, 0), (3, 3), (1, 1)], projectionCrossing)
                , ([(2, 1), (1, 3), (6, 1), (2, 2)], projectionCrossing)
                , ([(1, 2), (5, 0), (5, 3), (6, 2)], projectionCrossing)
                , ([(4, 1), (6, 0), (6, 3), (4, 2)], projectionCrossing)
                , ([(5, 1), (3, 2), (4, 3), (5, 2)], projectionCrossing)
                ]))

        , testCase "Test 4 legs planar part" $ do
            False @=? has4LegPlanarPart (implode (0,
                [ ([(1, 2), (1, 3), (1, 0), (1, 1)], projectionCrossing)
                ]))

            True @=? has4LegPlanarPart (implode (0,
                [ ([(2, 1), (2, 0), (3, 2), (3, 3)], projectionCrossing)
                , ([(1, 1), (1, 0), (3, 1), (3, 0)], projectionCrossing)
                , ([(2, 3), (2, 2), (1, 2), (1, 3)], projectionCrossing)
                ]))
        ]
