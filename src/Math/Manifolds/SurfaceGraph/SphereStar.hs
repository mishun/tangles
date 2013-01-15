module Math.Manifolds.SurfaceGraph.SphereStar
    ( sphereStarDecomposition
    ) where

import Math.Manifolds.SurfaceGraph


sphereStarDecomposition :: SurfaceGraph -> (SurfaceGraph, SurfaceGraph)
sphereStarDecomposition g
    | eulerChar g == 2  = error "sphereStarDecomposition: undefined for planar graphs"
    | otherwise         = error "sphereStarDecomposition: not implemented"
