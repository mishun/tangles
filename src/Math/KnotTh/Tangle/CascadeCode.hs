module Math.KnotTh.Tangle.CascadeCode
	( CascadeCodePattern(..)
	, decodeCascadeCode
	, ProjPattern(..)
	, Pattern(..)
	) where

import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating


class (Enum pattern, CrossingType ct) => CascadeCodePattern pattern ct | pattern -> ct where
	cascadeCodeRoot :: [(pattern, Int)] -> Tangle ct
	decodeCrossing  :: pattern -> (Int, CrossingState ct)


decodeCascadeCode :: (CascadeCodePattern p ct) => [(p, Int)] -> Tangle ct
decodeCascadeCode code =
	foldl
		(\ prev (pattern, offset) ->
			let (gl, c) = decodeCrossing pattern
			in if gl < 1 || gl > 3
				then error $ "decodeCascadeCode: expected 1, 2 or 3 for number of legs, found " ++ show gl
				else crossingTangle $ glueToBorder (nthLeg prev $ offset + gl - 1) gl c
		)
		(cascadeCodeRoot code)
		code


data ProjPattern = W | X | M deriving (Eq, Enum, Show)


instance Read ProjPattern where
	readsPrec _ s = case s of
		'W' : t -> [(W, t)]
		'X' : t -> [(X, t)]
		'M' : t -> [(M, t)]
		_       -> []


instance CascadeCodePattern ProjPattern ProjectionCrossing where
	cascadeCodeRoot _ = lonerProjection

	decodeCrossing W = (3, projectionCrossing)
	decodeCrossing X = (2, projectionCrossing)
	decodeCrossing M = (1, projectionCrossing)


data Pattern = WO | WU | XO | XU | MO | MU deriving (Eq, Enum)


instance Show Pattern where
	show p = case p of
		WO -> "W+"
		WU -> "W-"
		XO -> "X+"
		XU -> "X-"
		MO -> "M+"
		MU -> "M-"


instance Read Pattern where
	readsPrec _ s = case s of
		'W' : '+' : t -> [(WO, t)]
		'W' : '-' : t -> [(WU, t)]
		'X' : '+' : t -> [(XO, t)]
		'X' : '-' : t -> [(XU, t)]
		'M' : '+' : t -> [(MO, t)]
		'M' : '-' : t -> [(MU, t)]
		_             -> []


instance CascadeCodePattern Pattern ArbitraryCrossing where
	cascadeCodeRoot _ = lonerOverCrossing

	decodeCrossing WO = (3, underCrossing)
	decodeCrossing WU = (3, overCrossing)
	decodeCrossing XO = (2, overCrossing)
	decodeCrossing XU = (2, underCrossing)
	decodeCrossing MO = (1, overCrossing)
	decodeCrossing MU = (1, underCrossing)
