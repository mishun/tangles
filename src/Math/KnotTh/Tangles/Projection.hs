module Math.KnotTh.Tangles.Projection
	( module Math.KnotTh.Crossings
	, module Math.KnotTh.Crossings.Projection
	, module Math.KnotTh.Tangles
	, TangleProjection
	, lonerProjection
	, tangleProjection
	) where

import Math.KnotTh.Crossings
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Tangles


type TangleProjection = Tangle ProjectionCrossing


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing


tangleProjection :: (CrossingType ct) => Tangle ct -> TangleProjection
tangleProjection = mapCrossingStates (const projectionCrossing)
