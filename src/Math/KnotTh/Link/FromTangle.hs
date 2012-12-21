module Math.KnotTh.Link.FromTangle
	( tangleDoubling
	) where

import Math.Algebra.Group.D4 (D4, ec, (<*>))
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Link as L
import qualified Math.KnotTh.Tangle as T


tangleDoubling :: (CrossingType ct) => (D4 -> D4) -> T.Tangle ct -> L.Link ct
tangleDoubling f tangle =
	let resultLoops = 2 * (numberOfFreeLoops tangle) + div (length $ filter T.isLeg $ T.allLegOpposites tangle) 2
	in L.fromList (resultLoops, do
		let atTop d =
			let c = incidentCrossing d
			in (2 * crossingIndex c - 1, dartPlace d)
		let atBottom d =
			let c = incidentCrossing d
			in (2 * crossingIndex c, 3 - dartPlace d)

		a <- allCrossings tangle

		let top =
			let pair ab
				| T.isLeg ba  = atBottom ab
				| otherwise   = atTop ba
				where
					ba = opposite ab
			in (map pair $ incidentDarts a, crossingState a)

		let bottom =
			let pair ab
				| T.isLeg ba  = atTop ab
				| otherwise   = atBottom ba
				where
					ba = opposite ab
			in (map pair $ reverse $ incidentDarts a, mapOrientation ((ec <*>) . f) $ crossingState a)

		[top, bottom]
		)
