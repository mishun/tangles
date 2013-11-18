module Math.Topology.Manifolds.SurfaceGraph.Util
    ( isTriangulation
    ) where

import Math.Topology.Manifolds.SurfaceGraph.Definition


isTriangulation :: SurfaceGraph a -> Bool
isTriangulation = all ((== 3) . faceDegree) . allFaces
