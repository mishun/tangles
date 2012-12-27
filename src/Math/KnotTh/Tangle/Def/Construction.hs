module Math.KnotTh.Tangle.Def.Construction
	( lonerTangle
	, zeroTangle
	, infinityTangle
	, transformTangle
	) where

import Math.Algebra.Group.Dn (Dn, pointsUnderGroup, reflection, rotation, permute)
import Math.Algebra.Group.D4 ((<*>), ec)
import Math.KnotTh.Tangle.Def.Tangle


lonerTangle :: (CrossingType ct) => CrossingState ct -> Tangle ct
lonerTangle !cr = implode
	( 0
	, [(1, 0), (1, 1), (1, 2), (1, 3)]
	, [([(0, 0), (0, 1), (0, 2), (0, 3)], cr)]
	)


zeroTangle :: Tangle ct
zeroTangle = implode (0, [(0, 3), (0, 2), (0, 1), (0, 0)], [])


infinityTangle :: Tangle ct
infinityTangle = implode (0, [(0, 1), (0, 0), (0, 3), (0, 2)], [])


transformTangle :: (CrossingType ct) => Dn -> Tangle ct -> Tangle ct
transformTangle g tangle
	| l /= pointsUnderGroup g                   = error "transformTangle: order conflict"
	| reflection g == False && rotation g == 0  = tangle
	| otherwise                                 = implode (numberOfFreeLoops tangle, border, map crossing $ allCrossings tangle)
	where
		l = numberOfLegs tangle

		pair d
			| isLeg d    = (0, permute g $ legPlace d)
			| otherwise  =
				let c = incidentCrossing d
				in (crossingIndex c, if reflection g then 3 - dartPlace d else dartPlace d)

		crossing c
			| reflection g  = (reverse $ map pair $ adjacentDarts c, mapOrientation (ec <*>) $ crossingState c)
			| otherwise     = (map pair $ adjacentDarts c, crossingState c)

		border
			| reflection g  = head rotated : reverse (tail rotated)
			| otherwise     = rotated
			where
				rotated =
					let (pre, post) = splitAt (l - rotation g) $ map (pair . opposite) $ allLegs tangle
					in post ++ pre
