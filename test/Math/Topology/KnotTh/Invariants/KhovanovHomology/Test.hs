module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Text.Printf
import Math.Topology.KnotTh.Invariants.KhovanovHomology
import Math.Topology.KnotTh.Tangle


test :: Test
test = testGroup "Khovanov homology"
    [ testCase "Over crossing complex" $ do
        let c = overCrossingComplex
        assertBool "∂∘∂" $ testComplexBorders c

    , testCase "Under crossing complex" $ do
        let c = underCrossingComplex
        assertBool "∂∘∂" $ testComplexBorders c

    , testCase "Pair of crossings complex" $ do
        let c = horizontalComposition 2 (overCrossingComplex, 0) (underCrossingComplex, 0)
        assertBool "∂∘∂" $ testComplexBorders c

    , testCase "∂∘∂" $ do
        mapM_ (\ tangle -> do
                let kh = khovanovComplex tangle
                assertBool (printf "∂∘∂ failed at: %s" (show tangle)) $ testComplexBorders kh
            )
            [ toTangle lonerOverCrossing
            , toTangle lonerUnderCrossing
            , toTangle $ rationalTangle [2]
            , toTangle $ rationalTangle [3]
            , toTangle $ rationalTangle [-2]
            , toTangle $ rationalTangle [1, 2]
            , toTangle $ rationalTangle [2, 1, 1]
            , toTangle $ rationalTangle [-4]
            , toTangle $ rationalTangle [2, 3, -1]
            ]
    ]
