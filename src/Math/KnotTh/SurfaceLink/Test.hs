module Math.KnotTh.SurfaceLink.Test
    ( test
    ) where

import Math.KnotTh.SurfaceLink.Projection
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)


test :: Test
test = testGroup "SurfaceLink tests"
    [ testCase "Simplest torus link" $ do
        let l :: SurfaceLinkProjection
            l = implode (0, [([(1, 2), (1, 3), (1, 0), (1, 1)], projectionCrossing)])
        eulerChar l @?= 0
    ]
