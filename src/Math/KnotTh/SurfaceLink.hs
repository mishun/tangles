module Math.KnotTh.SurfaceLink
    ( module X
    , SurfaceLinkProjection
    , NonAlternatingSurfaceLink
    , NonAlternatingCrossing
    , NonAlternatingDart
    ) where

import Math.KnotTh.Knotted as X
import Math.KnotTh.SurfaceLink.Definition.SurfaceLink as X
import Math.KnotTh.SurfaceLink.Definition.EdgeIndicesEncoding as X
import Math.KnotTh.Crossings.Projection as X
import Math.KnotTh.Crossings.Arbitrary as X


type SurfaceLinkProjection = SurfaceLink ProjectionCrossing


type NonAlternatingSurfaceLink = SurfaceLink ArbitraryCrossing

type NonAlternatingCrossing = Crossing SurfaceLink  ArbitraryCrossing

type NonAlternatingDart = Dart SurfaceLink ArbitraryCrossing
