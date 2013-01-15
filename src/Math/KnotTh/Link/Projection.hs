module Math.KnotTh.Link.Projection
    ( module Math.KnotTh.Crossings.Projection
    , module Math.KnotTh.Link
    , LinkProjection
    ) where

import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Link


type LinkProjection = Link ProjectionCrossing
