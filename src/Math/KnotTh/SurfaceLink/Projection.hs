module Math.KnotTh.SurfaceLink.Projection
    ( module Math.KnotTh.Crossings.Projection
    , module Math.KnotTh.SurfaceLink
    , SurfaceLinkProjection
    ) where

import Math.KnotTh.Crossings.Projection
import Math.KnotTh.SurfaceLink


type SurfaceLinkProjection = SurfaceLink ProjectionCrossing
