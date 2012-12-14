module Math.KnotTh.Tangle.Projection
	( module Math.KnotTh.Crossings.Projection
	, module Math.KnotTh.Tangle
	, TangleProjection
	, lonerProjection
	) where

import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Tangle


type TangleProjection = Tangle ProjectionCrossing


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing
