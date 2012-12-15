module Math.KnotTh.Knotted.KnottedWithToPair
	( KnottedWithToPair(..)
	) where

import Math.KnotTh.Knotted.Knotted


class (Knotted knot cross dart) => KnottedWithToPair knot cross dart | knot -> cross, cross -> dart, dart -> knot where
	toPair :: dart ct -> (Int, Int)

	toPair d =
		let c = crossingIndex $ incidentCrossing d
		    p = dartPlace d
		in c `seq` p `seq` (c, p)
