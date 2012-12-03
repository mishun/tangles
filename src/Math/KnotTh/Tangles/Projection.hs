module Math.KnotTh.Tangles.Projection
	( module Math.KnotTh.Crossings.Projection
	, module Math.KnotTh.Tangles
	, TangleProjection
	, lonerProjection
	) where

import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Tangles


type TangleProjection = Tangle ProjectionCrossing


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing
