module Math.Topology.KnotTh.PlanarAlgebra
    ( PlanarAlgebra'(..)
    ) where


class PlanarAlgebra' a where
    numberOfLegs          :: a -> Int
    planarPropagator      :: Int -> a
    planarRotate          :: Int -> a -> a
    horizontalComposition :: Int -> (a, Int) -> (a, Int) -> a
    planarLoop            :: a

    planarLoop = horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)
