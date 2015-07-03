module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( KhovanovComplex(..)
    , testBorders
    , overCrossingComplex
    , underCrossingComplex
    ) where

import Math.Topology.KnotTh.Cobordism.CobordismMatrix
import Math.Topology.KnotTh.Cobordism.DottedCobordism


data KhovanovComplex c =
    KhovanovComplex
        { dimOffset :: Int
        , borders   :: [CobordismMatrix c]
        }

testBorders :: (PreadditiveCobordism c) => KhovanovComplex c -> Bool
testBorders comp = all isZeroCobordism $ zipWith (∘) (tail $ borders comp) (borders comp)


overCrossingComplex, underCrossingComplex :: KhovanovComplex (DottedCobordism Integer)
overCrossingComplex =
    KhovanovComplex
        { dimOffset = 0
        , borders   = [singleton saddleCobordism]
        }
underCrossingComplex =
    KhovanovComplex
        { dimOffset = 0
        , borders   = [singleton saddleCobordism']
        }