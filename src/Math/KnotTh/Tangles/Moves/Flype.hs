module Math.KnotTh.Tangles.Moves.Flype
	( neighbours
	) where

import Data.Array ((!))
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move
import Math.KnotTh.Tangles.Util.Resting


neighbours :: NonAlterantingTangle -> [(NonAlterantingTangle, Int)]
neighbours tangle = mapMaybe tryFlype $ allDarts tangle
	where
		tryFlype ab
			| isLeg ba || isLeg ca        = Nothing
			| b == c || a == b || a == c  = Nothing
			| isNothing flype2T           = Nothing
			| otherwise                   =
				Just $ moveZ tangle $ do
					substituteM [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
					connectM [(rp, ae), (sq, ad)]
					flipM $ filter ((!) sub) $ allCrossings tangle
			where
				ba = opposite ab
				ac = nextCCW ab
				ca = opposite ac

				a = incidentCrossing ab
				b = incidentCrossing ba
				c = incidentCrossing ca

				ae = nextCCW ac
				ad = nextCW ab

				flype2T =
					let otherPair (lst, s) =
						case lst of
							[x, y] -> Just $! ((x, y), s)
							_      -> Nothing
					in restingPart tangle [ba, ca] >>= otherPair

				((rp, sq), sub) = fromJust flype2T
