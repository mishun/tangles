module Math.KnotTh.Crossings.Projection
	( ProjectionCrossing(..)
	, projectionCrossing
	, projectionCrossings
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
