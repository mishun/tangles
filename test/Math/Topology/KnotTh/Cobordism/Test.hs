{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Math.Topology.KnotTh.Cobordism.Test
    ( generalCobordism3Tests
    , generalCannedCobordismTests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Cobordism
import Math.Topology.KnotTh.Dihedral (rotateBy)
import Math.Topology.KnotTh.PlanarAlgebra


generalCobordism3Tests :: forall cob. (Cobordism3 cob, Show cob, Show (CobordismBorder cob)) => cob -> Test
generalCobordism3Tests _ =
    let (=?~=) = (@?=) :: cob -> cob -> Assertion
        (=~?=) = (@=?) :: cob -> cob -> Assertion
    in testGroup "General cobordism properties tests"
        [ testCase "Trivial border properties" $ do
            let cap = capCobordism :: cob
            numberOfLoops (cobordismBorder0 cap) @?= 1
            numberOfLoops (cobordismBorder1 cap) @?= 0

            let cup = cupCobordism :: cob
            numberOfLoops (cobordismBorder0 cup) @?= 0
            numberOfLoops (cobordismBorder1 cup) @?= 1
            cobordismBorder0 cap @?= cobordismBorder1 cup
            cobordismBorder0 cup @?= cobordismBorder1 cap

            let tube = tubeCobordism :: cob
            cobordismBorder1 tube @?= cobordismBorder0 tube
            cobordismBorder0 tube @?= cobordismBorder0 cap
            cobordismBorder0 tube @?= cobordismBorder1 cup

            let sphere = sphereCobordism :: cob
            cobordismBorder1 sphere @?= cobordismBorder0 sphere
            cobordismBorder0 sphere @?= cobordismBorder1 cap
            cobordismBorder0 sphere @?= cobordismBorder0 cup

            let torus = torusCobordism :: cob
            cobordismBorder0 torus @?= cobordismBorder0 sphere
            cobordismBorder1 torus @?= cobordismBorder1 sphere

            let pants = pantsCobordism :: cob
            numberOfLoops (cobordismBorder0 pants) @?= 2
            cobordismBorder1 pants @?= cobordismBorder1 cup
            cobordismBorder0 pants @?= cobordismBorder0 (cap ⊗ cap)

            let swap = swapCobordism :: cob
            cobordismBorder0 swap @?= cobordismBorder0 pants
            cobordismBorder1 swap @?= cobordismBorder0 pants

        , testCase "Flip" $ do
            flipCobordism capCobordism =?~= cupCobordism
            flipCobordism cupCobordism =?~= capCobordism
            flipCobordism tubeCobordism =?~= tubeCobordism
            flipCobordism sphereCobordism =?~= sphereCobordism
            flipCobordism torusCobordism =?~= torusCobordism
            flipCobordism pantsCobordism' =?~= pantsCobordism

        , testCase "cap ∘ cup" $ do
            (capCobordism ∘ tubeCobordism ∘ cupCobordism) =?~= sphereCobordism

        , testCase "cup ∘ cap" $ do
            let r = cupCobordism ∘ capCobordism
            (r ∘ r) =?~= (r ⊗ sphereCobordism)

        , testCase "Tube is identity" $ do
            (capCobordism ∘ tubeCobordism) =?~= capCobordism
            (tubeCobordism ∘ cupCobordism) =?~= cupCobordism
            (tubeCobordism ∘ tubeCobordism) =?~= tubeCobordism
            (capCobordism ∘ tubeCobordism ∘ cupCobordism) =?~= (capCobordism ∘ cupCobordism)
            (tubeCobordism ∘ pantsCobordism ∘ (tubeCobordism ⊗ tubeCobordism)) =?~= pantsCobordism
            ((tubeCobordism ⊗ tubeCobordism) ∘ (tubeCobordism ⊗ tubeCobordism)) =?~= (tubeCobordism ⊗ tubeCobordism)

        , testCase "Swap is idempotent" $ do
            (swapCobordism ∘ swapCobordism) =?~= (tubeCobordism ⊗ tubeCobordism)

        , testCase "Torus from pants and caps/cups" $ do
            torusCobordism =~?=
                foldl1 (∘)
                    [ capCobordism
                    , pantsCobordism
                    , tubeCobordism ⊗ tubeCobordism
                    , pantsCobordism'
                    , cupCobordism
                    ]

        , testCase "Sphere from pants and caps/cups" $ do
            sphereCobordism =~?=
                foldl1 (∘)
                    [ capCobordism   ⊗ capCobordism
                    , pantsCobordism ⊗ tubeCobordism
                    , tubeCobordism  ⊗ pantsCobordism'
                    , cupCobordism   ⊗ cupCobordism
                    ]
        ]


generalCannedCobordismTests :: forall cob. (CannedCobordism cob, Show cob, Show (CobordismBorder cob)) => cob -> Test
generalCannedCobordismTests _ =
    let (=?~=) = (@?=) :: cob -> cob -> Assertion
    in testGroup "General canned cobordism properties tests"
        [ testCase "numberOfLegs" $ do
            planarDegree (capCobordism :: cob) @?= 0
            planarDegree (cupCobordism :: cob) @?= 0
            planarDegree (tubeCobordism :: cob) @?= 0
            planarDegree (sphereCobordism :: cob) @?= 0
            planarDegree (torusCobordism :: cob) @?= 0
            planarDegree (pantsCobordism :: cob) @?= 0

            let saddle = saddleCobordism :: cob
            planarDegree saddle @?= 4
            numberOfLoops (cobordismBorder0 saddle) @?= 0
            numberOfLoops (cobordismBorder1 saddle) @?= 0
            cobordismBorder0 saddle @?= planarPropagator 2
            cobordismBorder1 saddle @?= rotateBy 1 (planarPropagator 2)

        , testCase "Tube in many ways" $ do
            identityCobordism (horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)) =?~= tubeCobordism
            horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0) =?~= tubeCobordism
            horizontalComposition 6 (planarPropagator 3, 0) (planarPropagator 3, 5) =?~= tubeCobordism

        , testCase "Flip" $ do
            flipCobordism saddleCobordism =?~= saddleCobordism'
            flipCobordism sideCutPantsCobordism =?~= sideCutPantsCobordism'

        , testCase "Rotate saddle" $ do
            rotateBy 1 saddleCobordism =?~= saddleCobordism'
            rotateBy 2 saddleCobordism =?~= saddleCobordism

        , testCase "Saddle and identity" $ do
            identityCobordism (rotateBy 1 $ planarPropagator 2) ∘ saddleCobordism ∘ planarPropagator 2 =?~= saddleCobordism

        , testCase "Pants from saddle" $ do
            horizontalComposition 4 (saddleCobordism, 0) (planarPropagator 2, 0) =?~= pantsCobordism
            horizontalComposition 4 (saddleCobordism', 0) (planarPropagator 2, 0) =?~= pantsCobordism'

        , testCase "Handle formation" $ do
            let g = horizontalComposition 2 (saddleCobordism, 0) (planarPropagator 1, 0)
            capCobordism ∘ horizontalComposition 2 (planarPropagator 1, 0) (flipCobordism g ∘ g, 0) ∘ cupCobordism =?~= torusCobordism

        , testCase "Triple pants" $ do
            (pantsCobordism ∘ (tubeCobordism ⊗ pantsCobordism)) =?~= horizontalComposition 2 (sideCutPantsCobordism, 0) (sideCutPantsCobordism, 0)
        ]
