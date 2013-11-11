module Math.Topology.KnotTh.SurfaceLink
    ( module X
    , SurfaceLinkProj
    , SurfaceLinkProjCrossing
    , SurfaceLinkProjDart
    , NASurfaceLink
    , NASurfaceLinkCrossing
    , NASurfaceLinkDart
    ) where

import Math.Topology.KnotTh.Knotted as X
import Math.Topology.KnotTh.SurfaceLink.Definition.SurfaceLink as X
import Math.Topology.KnotTh.SurfaceLink.Definition.EdgeIndicesEncoding as X
import Math.Topology.KnotTh.Crossings.Projection as X
import Math.Topology.KnotTh.Crossings.Arbitrary as X


type SurfaceLinkProj = SurfaceLink ProjectionCrossing
type SurfaceLinkProjCrossing = Crossing SurfaceLink ProjectionCrossing
type SurfaceLinkProjDart = Dart SurfaceLink ProjectionCrossing


type NASurfaceLink = SurfaceLink ArbitraryCrossing
type NASurfaceLinkCrossing = Crossing SurfaceLink ArbitraryCrossing
type NASurfaceLinkDart = Dart SurfaceLink ArbitraryCrossing
