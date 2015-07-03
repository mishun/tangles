module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Invariants.KhovanovHomology


test :: Test
test = testGroup "Khovanov homology"
    [ testCase "Basic complexes" $ do
        assertBool "dir" $ testBorders overCrossingComplex
        assertBool "rev" $ testBorders underCrossingComplex
    ]
