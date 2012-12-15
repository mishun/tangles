module Math.KnotTh.Tangle.CascadeCode
	( CascadeCodePattern(..)
	, decodeCascadeCode
	, ProjPattern(..)
	, DiagPattern(..)
	) where

import Data.Char (isSpace)
import Math.Algebra.Group.Dn (fromRotation)
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating


class (Enum pattern, CrossingType ct) => CascadeCodePattern pattern ct | pattern -> ct where
	cascadeCodeRoot :: [(pattern, Int)] -> Tangle ct
	decodeCrossing  :: pattern -> (ProjPattern, Int, Int, CrossingState ct)


decodeCascadeCode :: (CascadeCodePattern p ct) => [(p, Int)] -> Tangle ct
decodeCascadeCode code =
	foldl
		(\ prev (pattern, offset) ->
			let	(gl, shift, rot, c) = decodeCrossing pattern
				p	| rot == 0   = id
					| otherwise  = \ t ->
						let l = numberOfLegs t
						in transformTangle (fromRotation l rot) t
			in p $ crossingTangle $ glueToBorder
				(nthLeg prev $ offset + shift)
				(case gl of { W -> 3 ; X -> 2 ; M -> 1 })
				c
		)
		(cascadeCodeRoot code)
		code


data ProjPattern = W | X | M deriving (Eq, Enum, Show)


instance Read ProjPattern where
	readsPrec _ s = case dropWhile isSpace s of
		'W' : t -> [(W, t)]
		'X' : t -> [(X, t)]
		'M' : t -> [(M, t)]
		_       -> []


instance CascadeCodePattern ProjPattern ProjectionCrossing where
	cascadeCodeRoot _ = lonerProjection

	decodeCrossing W = (W, 1, 0, projectionCrossing)
	decodeCrossing X = (X, 1, 0, projectionCrossing)
	decodeCrossing M = (M, 0, -1, projectionCrossing)


data DiagPattern = WO | WU | XO | XU | MO | MU deriving (Eq, Enum)


instance Show DiagPattern where
	show p = case p of
		WO -> "W+"
		WU -> "W-"
		XO -> "X+"
		XU -> "X-"
		MO -> "M+"
		MU -> "M-"


instance Read DiagPattern where
	readsPrec _ s = case dropWhile isSpace s of
		'W' : '+' : t -> [(WO, t)]
		'W' : '-' : t -> [(WU, t)]
		'X' : '+' : t -> [(XO, t)]
		'X' : '-' : t -> [(XU, t)]
		'M' : '+' : t -> [(MO, t)]
		'M' : '-' : t -> [(MU, t)]
		_             -> []


instance CascadeCodePattern DiagPattern ArbitraryCrossing where
	cascadeCodeRoot _ = lonerOverCrossing

	decodeCrossing WO = (W, 1, 0, underCrossing)
	decodeCrossing WU = (W, 1, 0, overCrossing)
	decodeCrossing XO = (X, 1, 0, overCrossing)
	decodeCrossing XU = (X, 1, 0, underCrossing)
	decodeCrossing MO = (M, 0, -1, overCrossing)
	decodeCrossing MU = (M, 0, -1, underCrossing)
