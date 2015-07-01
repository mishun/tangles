module Math.Topology.KnotTh.Cobordism.DottedCobordism.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, test)
import Test.QuickCheck
import Math.Topology.KnotTh.Cobordism.DottedCobordism
import Math.Topology.KnotTh.Cobordism.Test


type Cob = DottedCobordism Integer

test :: Test
test = testGroup "Dotted cobordisms"
    [ generalCobordism3Tests (undefined :: Cob)
    , generalCannedCobordismTests (undefined :: Cob)

    , let (=?~=) = (@?=) :: Cob -> Cob -> Assertion
      in testGroup "Algebraic propeties"
        [ testCase "Sphere" $ do
            sphereCobordism =?~= 0

        , testCase "Torus" $ do
            torusCobordism  =?~= 2

        , testProperty "Closed surface" $ \ genus ->
            genus >= 0 ==> (surfOfGenusCobordism genus) == ((if genus == 1 then 2 else 0) :: Cob)

        , testCase "4Tu relation" $ do
            let cap = capCobordism
                cup = cupCobordism
                tube = tubeCobordism
                arc = cap ∘ pantsCobordism
                arc' = flipCobordism arc
                cut = cup ∘ cap
            (((cup ⊗ cup) ∘ arc) + (arc' ∘ (cap ⊗ cap))) =?~= ((tube ⊗ cut) + (cut ⊗ tube))
            ((arc ∘ (tube ⊗ cap ⊗ cap ⊗ tube)) + (cap ⊗ arc ⊗ cap)) =?~= ((arc ⊗ cap ⊗ cap) + (cap ⊗ cap ⊗ arc))

        , testCase "Neck cutting relation" $ do
            (tubeCobordism + tubeCobordism) =?~= ((cupOfGenusCobordism 1 ∘ capCobordism) + (cupCobordism ∘ capOfGenusCobordism 1))

        , testCase "Reidemeister I homotopy" $ do
            let f0 = (planarPropagator 1 ⊗ cupOfGenusCobordism 1) - sideCutPantsCobordism'
                g0 = planarPropagator 1 ⊗ capCobordism
                d = sideCutPantsCobordism
                h = planarPropagator 1 ⊗ cupCobordism
            (d ∘ f0) =?~= (0 ⊗ planarPropagator 1)
            ((f0 ∘ g0) + (h ∘ d)) =?~= (planarPropagator 1 ⊗ tubeCobordism)
        ]
    ]
