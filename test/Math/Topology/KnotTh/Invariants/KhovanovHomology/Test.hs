module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Invariants.KhovanovHomology
import Math.Topology.KnotTh.Tangle


test :: Test
test = testGroup "Khovanov homology"
    [ testCase "Over crossing complex" $ do
        assertBool "dir" $ testComplexBorders overCrossingComplex

    , testCase "Under crossing complex" $ do
        assertBool "rev" $ testComplexBorders underCrossingComplex

    , testCase "Pair of crossings complex" $ do
        assertBool "pair" $ testComplexBorders $ horizontalComposition 2 (overCrossingComplex, 0) (underCrossingComplex, 0)

    , testCase "∂" $
        mapM_ (\ tangle -> assertBool (show tangle) $ testComplexBorders $ khovanovComplex tangle)
            [ toTangle lonerOverCrossing
            -- , toTangle lonerUnderCrossing
            -- , toTangle $ rationalTangle [2]
            -- , toTangle $ rationalTangle [2, 3, -1]
            ]
    ]
