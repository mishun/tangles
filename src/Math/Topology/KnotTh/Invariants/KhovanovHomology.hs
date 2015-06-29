module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( KhovanovComplex(..)
    ) where

import qualified Data.Vector as V
import Math.Topology.KnotTh.Cobordism.DottedCobordism


data KhovanovComplex a =
    KhovanovComplex
        { dimOffset :: Int
        , borders   :: [V.Vector (V.Vector (DottedCobordism a))]
        }
