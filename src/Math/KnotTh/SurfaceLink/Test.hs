module Math.KnotTh.SurfaceLink.Test
    ( test
    ) where

import Control.Monad (forM_)
import Math.KnotTh.SurfaceLink.Projection
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)


test :: Test
test =
    let testCorr link = do
            forM_ (allHalfEdges link) $ \ d -> do
                let f = faceToTheLeft d
                    p = placeToTheLeft d
                nthCCWBorderDart f p @?= d

            forM_ (allHalfEdges link) $ \ d -> do
                let f = faceToTheRight d
                    p = placeToTheRight d
                nthCWBorderDart f p @?= d

    in testGroup "SurfaceLink tests"
        [ testCase "Simplest torus link" $ do
            let l :: SurfaceLinkProjection
                l = implode (0, [([(1, 2), (1, 3), (1, 0), (1, 1)], projectionCrossing)])

            testCorr l
            assertEqual "Number of faces" 1 $ numberOfFaces l
            assertEqual "Face degree" 4 $ faceDegree (nthFace l 1)
            assertEqual "Euler char" 0 $ eulerChar l
        ]
