module Math.Topology.Manifolds.SurfaceGraph
    ( module Def
    , module Util
    , module Bary
    , module E
    , module SS
    , Cell(..)
    ) where

import Math.Topology.Manifolds.SurfaceGraph.Definition as Def
import Math.Topology.Manifolds.SurfaceGraph.Util as Util
import Math.Topology.Manifolds.SurfaceGraph.Barycentric as Bary
import Math.Topology.Manifolds.SurfaceGraph.Embedding as E
import Math.Topology.Manifolds.SurfaceGraph.SphereStar as SS


data Cell a = Cell0D (Vertex SurfaceGraph a) | Cell1D (Dart SurfaceGraph a) | Cell2D (Face SurfaceGraph a) deriving (Eq, Ord)
