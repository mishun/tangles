module Math.KnotTh.Crossings.Projection
	( ProjectionCrossing(..)
	, projectionCrossing
	, projectionCrossings
	, projection
	) where

import Control.DeepSeq
import Math.Algebra.Group.D4 (subGroupD4)
import Math.KnotTh.Knotted


data ProjectionCrossing = ProjectionCrossing deriving (Eq)


instance NFData ProjectionCrossing


instance CrossingType ProjectionCrossing where
	localCrossingSymmetry _ = subGroupD4

	possibleOrientations _ _ = projectionCrossings


instance Show ProjectionCrossing where
	show _ = "+"


projectionCrossing :: CrossingState ProjectionCrossing
projectionCrossing = makeCrossing' ProjectionCrossing


projectionCrossings :: [CrossingState ProjectionCrossing]
projectionCrossings = [projectionCrossing]


projection :: (CrossingType ct, Knotted k c d) => k ct -> k ProjectionCrossing
projection = mapCrossings (const projectionCrossing)
