{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Math.Topology.KnotTh.Algebra.Cobordism.Test
    ( generalCobordism3Tests
    , generalCannedCobordismTests
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Algebra.Cobordism
import Math.Topology.KnotTh.Algebra.Dihedral
import Math.Topology.KnotTh.Algebra.PlanarAlgebra


generalCobordism3Tests :: forall cob. (Cobordism3 cob, Eq cob, Show cob, Show (CobordismBorder cob)) => cob -> Test
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

            let tube = tubeCobordism :: cob
            numberOfLoops (cobordismBorder0 tube) @?= 1
            numberOfLoops (cobordismBorder1 tube) @?= 1
            cobordismBorder1 tube @?= cobordismBorder0 tube

            let sphere = sphereCobordism :: cob
            numberOfLoops (cobordismBorder0 sphere) @?= 0
            numberOfLoops (cobordismBorder1 sphere) @?= 0

            let torus = torusCobordism :: cob
            numberOfLoops (cobordismBorder0 torus) @?= 0
            numberOfLoops (cobordismBorder1 torus) @?= 0

            let pants = pantsCobordism :: cob
            numberOfLoops (cobordismBorder0 pants) @?= 2
            numberOfLoops (cobordismBorder1 pants) @?= 1

            let swap = swapCobordism :: cob
            numberOfLoops (cobordismBorder0 swap) @?= 2
            numberOfLoops (cobordismBorder1 swap) @?= 2

--        , testCase "Transpose" $ do
--            transposeIt capCobordism =?~= cupCobordism
--            transposeIt cupCobordism =?~= capCobordism
--            transposeIt tubeCobordism =?~= tubeCobordism
--            transposeIt sphereCobordism =?~= sphereCobordism
--            transposeIt torusCobordism =?~= torusCobordism
--            transposeIt pantsCobordism' =?~= pantsCobordism

--        , testCase "cap ∘ cup" $
--            (capCobordism ∘ tubeCobordism ∘ cupCobordism) =?~= sphereCobordism

--        , testCase "cup ∘ cap" $ do
--            let r = cupCobordism ∘ capCobordism
--            (r ∘ r) =?~= (r ⊗ sphereCobordism)

        , testCase "Tube is identity" $ do
            (capCobordism ∘ tubeCobordism) =?~= capCobordism
            (tubeCobordism ∘ cupCobordism) =?~= cupCobordism
            (tubeCobordism ∘ tubeCobordism) =?~= tubeCobordism
            (capCobordism ∘ tubeCobordism ∘ cupCobordism) =?~= (capCobordism ∘ cupCobordism)
--            (tubeCobordism ∘ pantsCobordism ∘ (tubeCobordism ⊗ tubeCobordism)) =?~= pantsCobordism
--            ((tubeCobordism ⊗ tubeCobordism) ∘ (tubeCobordism ⊗ tubeCobordism)) =?~= (tubeCobordism ⊗ tubeCobordism)

        , testCase "Swap is idempotent" $
            (swapCobordism ∘ swapCobordism) =?~= (tubeCobordism ⊗ tubeCobordism)

--        , testCase "Torus from pants and caps/cups" $
--            torusCobordism =~?=
--                foldl1 (∘)
--                    [ capCobordism
--                    , pantsCobordism
--                    , tubeCobordism ⊗ tubeCobordism
--                    , pantsCobordism'
--                    , cupCobordism
--                    ]

--        , testCase "Sphere from pants and caps/cups" $
--            sphereCobordism =~?=
--                foldl1 (∘)
--                    [ capCobordism   ⊗ capCobordism
--                    , pantsCobordism ⊗ tubeCobordism
--                    , tubeCobordism  ⊗ pantsCobordism'
--                    , cupCobordism   ⊗ cupCobordism
--                    ]
        ]


generalCannedCobordismTests :: forall cob. (CannedCobordism cob, Eq cob, Show cob, Show (CobordismBorder cob)) => cob -> Test
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

        , testCase "Tube in many ways" $ do
            identityCobordism (horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)) =?~= tubeCobordism
            horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0) =?~= tubeCobordism
            horizontalComposition 6 (planarPropagator 3, 0) (planarPropagator 3, 5) =?~= tubeCobordism

        , testCase "Transpose" $ do
            transposeIt saddleCobordism =?~= saddleCobordism'
            transposeIt sideCutPantsCobordism =?~= sideCutPantsCobordism'

        , testCase "Rotate saddle" $ do
            rotateBy 1 saddleCobordism =?~= saddleCobordism'
            rotateBy 2 saddleCobordism =?~= saddleCobordism

--        , testCase "Saddle and identity" $
--            identityCobordism (rotateBy 1 $ planarPropagator 2) ∘ saddleCobordism ∘ planarPropagator 2 =?~= saddleCobordism

        , testCase "Pants from saddle" $ do
            horizontalComposition 4 (saddleCobordism, 0) (planarPropagator 2, 0) =?~= pantsCobordism
            horizontalComposition 4 (saddleCobordism', 0) (planarPropagator 2, 0) =?~= pantsCobordism'

--        , testCase "Handle formation" $ do
--            let g = horizontalComposition 2 (saddleCobordism, 0) (planarPropagator 1, 0)
--            capCobordism ∘ horizontalComposition 2 (planarPropagator 1, 0) (transposeIt g ∘ g, 0) ∘ cupCobordism =?~= torusCobordism

--        , testCase "Triple pants" $
--            (pantsCobordism ∘ (tubeCobordism ⊗ pantsCobordism)) =?~= horizontalComposition 2 (sideCutPantsCobordism, 0) (sideCutPantsCobordism, 0)

--        , testCase "Triple saddle and associativity" $ do
--            let a0 = planarPropagator 1 ⊗ saddleCobordism
--                a1 = saddleCobordism' ⊗ planarPropagator 1
--                b0 = saddleCobordism ⊗ planarPropagator 1
--                b1 = rotateBy 2 (planarPropagator 1 ⊗ saddleCobordism')
--            ((b1 ∘ b0) ∘ (a1 ∘ a0)) =?~= (b1 ∘ (b0 ∘ (a1 ∘ a0)))
        ]
