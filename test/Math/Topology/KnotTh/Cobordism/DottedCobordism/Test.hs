module Math.Topology.KnotTh.Cobordism.DottedCobordism.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Cobordism.DottedCobordism
import Math.Topology.KnotTh.Cobordism.Test


type Cob = DottedCobordism Integer

test :: Test
test = testGroup "Dotted cobordisms"
    [ generalCobordismTests (undefined :: Cob)
    , generalCannedCobordismTests (undefined :: Cob)

    , let (=?~=) = (@?=) :: Cob -> Cob -> Assertion
      in testGroup "Algebraic propeties"
        [ testCase "Sphere" $ do
            sphereCobordism =?~= 0

        , testCase "Torus" $ do
            torusCobordism  =?~= 2

        , testCase "Reidemeister I homotopy" $ do
            let f0 = (planarPropagator 1 ⊗ cupCobordism' 1) - sideCutPantsCobordism'
                g0 = planarPropagator 1 ⊗ capCobordism
                d = sideCutPantsCobordism
                h = planarPropagator 1 ⊗ cupCobordism
            (d ∘ f0) =?~= (0 ⊗ planarPropagator 1)
            ((f0 ∘ g0) + (h ∘ d)) =?~= (planarPropagator 1 ⊗ tubeCobordism)
        ]
    ]
