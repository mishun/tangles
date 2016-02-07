module Math.Topology.KnotTh.SurfaceGraph
    ( module X
    , Cell(..)
    , isTriangulation
    ) where

import Math.Topology.KnotTh.SurfaceGraph.SurfaceGraphDef as X
import Math.Topology.KnotTh.SurfaceGraph.SphereStar as X


data Cell a = Cell0D (Vertex SurfaceGraph a)
            | Cell1D (Dart SurfaceGraph a)
            | Cell2D (Face SurfaceGraph a)
    deriving (Eq, Ord)


isTriangulation :: SurfaceGraph a -> Bool
isTriangulation = all ((== 3) . faceDegree) . allFaces
