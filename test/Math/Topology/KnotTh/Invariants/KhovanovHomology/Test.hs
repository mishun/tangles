module Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Invariants.KhovanovHomology


test :: Test
test = testGroup "Tangle generators"
    [ testCase "Loop from 2 propagators" $ do
        let loop :: CobordismBorder (DottedCobordism Integer)
            (loop, _, _) = glue 2 (planarPropagator, 0) (planarPropagator, 0)
        numberOfLegs loop @?= 0
        numberOfLoops loop @?= 1

    , testCase "cap ∘ cup" $ do
        let r :: DottedCobordism Integer
            r = capCobordism ∘ cupCobordism
        numberOfLegs r @?= 0
        numberOfLoops (cobordismBorder0 r) @?= 0
        numberOfLoops (cobordismBorder1 r) @?= 0

    , testCase "cup ∘ cap" $ do
            let r :: DottedCobordism Integer
                r = cupCobordism ∘ capCobordism
            numberOfLegs r @?= 0
            numberOfLoops (cobordismBorder0 r) @?= 1
            numberOfLoops (cobordismBorder1 r) @?= 1

    , testCase "Tube identity" $ do
        let tube1, tube2 :: DottedCobordism Integer
            (tube1, _, _) = glue 2 (planarPropagator, 0) (planarPropagator, 0)
            tube2 = identityCobordism $
                let (loop, _, _) = glue 2 (planarPropagator, 0) (planarPropagator, 0)
                in loop
        tube1 @?= (tube1 ∘ tube1)
        tube2 @?= (tube2 ∘ tube2)
        tube1 @?= tube2
    ]
