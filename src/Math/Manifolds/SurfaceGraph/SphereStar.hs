module Math.Manifolds.SurfaceGraph.SphereStar
    ( sphereStarDecomposition
    ) where

import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.SphereStar.Backtrack


sphereStarDecomposition :: SurfaceGraph -> (SurfaceGraph, SurfaceGraph)
sphereStarDecomposition g
    | eulerChar g == 2  = error "sphereStarDecomposition: undefined for planar graphs"
    | otherwise         =
        let (faceMarks, edgeMarks) = backtrack g
        in edgeMarks `seq` (g, g)
