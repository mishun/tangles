module Math.Topology.KnotTh.PlanarAlgebra
    ( PlanarAlgebra'(..)
    ) where

import Math.Topology.KnotTh.Dihedral


class (RotationAction a) => PlanarAlgebra' a where
    numberOfLegs          :: a -> Int
    planarPropagator      :: Int -> a
    horizontalComposition :: Int -> (a, Int) -> (a, Int) -> a
    planarLoop            :: a

    planarLoop = horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)
