module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Invariants.KhovanovHomology


type Cob = DottedCobordism Integer

test :: Test
test = testGroup "Tangle generators"
    [ testCase "Loop from 2 propagators" $ do
        let loop = horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0) :: Cob
        numberOfLegs (cobordismBorder0 loop) @?= 0
        numberOfLoops (cobordismBorder0 loop) @?= 1
        cobordismBorder1 loop @?= cobordismBorder0 loop

    , testCase "cap & cup" $ do
        let cap = capCobordism :: Cob
            cup = cupCobordism :: Cob
        flipCobordism cap @?= cup
        flipCobordism cup @?= cap

    , testCase "cap ∘ cup" $ do
        let r = capCobordism ∘ cupCobordism :: Cob
        numberOfLegs r @?= 0
        numberOfLoops (cobordismBorder0 r) @?= 0
        numberOfLoops (cobordismBorder1 r) @?= 0

    , testCase "cup ∘ cap" $ do
        let r = cupCobordism ∘ capCobordism :: Cob
        numberOfLegs r @?= 0
        numberOfLoops (cobordismBorder0 r) @?= 1
        numberOfLoops (cobordismBorder1 r) @?= 1

    , testCase "Propagator commutes with identityCobordism" $ do
        let tube1 = horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0) :: Cob
            tube2 = identityCobordism $ horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)
        tube1 @?= tubeCobordism
        tube2 @?= tubeCobordism
        tube1 @?= tube2

    , testCase "Tube is identity" $ do
        let tube = tubeCobordism :: Cob
        flipCobordism tube @?= tube
        capCobordism ∘ tube @?= capCobordism
        tube ∘ cupCobordism @?= cupCobordism
        tube ∘ tube @?= tube
        capCobordism ∘ tube ∘ cupCobordism @?= capCobordism ∘ cupCobordism

    , testCase "Multiple chords to single loop" $ do
        let p3 = planarPropagator 3 :: Cob
            tube = horizontalComposition 6 (p3, 0) (p3, 5)
        tube @?= tubeCobordism

    , testCase "Tube and tensor product" $ do
        let tube2 = tubeCobordism ⊗ tubeCobordism :: Cob
        tube2 ∘ tube2 @?= tube2

    , testCase "Saddle borders" $ do
        let saddle = saddleCobordism :: Cob
        cobordismBorder0 saddle @?= planarPropagator 2
        cobordismBorder1 saddle @?= planarRotate 1 (planarPropagator 2)
    ]
