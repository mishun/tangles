module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Invariants.KhovanovHomology


test :: Test
test = testGroup "Khovanov homology"
    [ testCase "Over crossing complex" $ do
        assertBool "dir" $ testBorders overCrossingComplex

    , testCase "Under crossing complex" $ do
        assertBool "rev" $ testBorders underCrossingComplex
    ]
