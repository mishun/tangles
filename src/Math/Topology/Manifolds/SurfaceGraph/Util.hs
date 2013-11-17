module Math.Topology.Manifolds.SurfaceGraph.Util
    ( verticesRangeG
    , facesRangeG
    , dartsRangeG
    , isTriangulation
    ) where

import Math.Topology.Manifolds.SurfaceGraph.Definition


verticesRangeG :: SurfaceGraph a -> (Vertex SurfaceGraph a, Vertex SurfaceGraph a)
verticesRangeG g = (nthVertex g 0, nthVertex g $ numberOfVertices g - 1)


facesRangeG :: SurfaceGraph a -> (Face SurfaceGraph a, Face SurfaceGraph a)
facesRangeG g = (nthFace g 0, nthFace g $ numberOfFaces g - 1)


dartsRangeG :: SurfaceGraph a -> (Dart SurfaceGraph a, Dart SurfaceGraph a)
dartsRangeG g = (nthDart g 0, nthDart g $ numberOfDarts g - 1)


isTriangulation :: SurfaceGraph a -> Bool
isTriangulation = all ((== 3) . faceDegree) . allFaces
