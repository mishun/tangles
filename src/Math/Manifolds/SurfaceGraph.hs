module Math.Manifolds.SurfaceGraph
    ( module Def
    , module Util
    , module Bary
    , module E
    , module SS

    , Cell(..)
    ) where

import Math.Manifolds.SurfaceGraph.Definition as Def
import Math.Manifolds.SurfaceGraph.Util as Util
import Math.Manifolds.SurfaceGraph.Barycentric as Bary
import Math.Manifolds.SurfaceGraph.Embedding as E
import Math.Manifolds.SurfaceGraph.SphereStar as SS


data Cell = Cell0D Vertex | Cell1D Dart | Cell2D Face deriving (Eq, Ord)
