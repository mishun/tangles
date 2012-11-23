module Math.KnotTh.Tangles.Moves.Flype
	( neighbours
	) where

import Data.Maybe
import Data.Array.Unboxed ((!))
import Control.Monad (when)
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move
import Math.KnotTh.Tangles.Moves.Resting


neighbours :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighbours tangle = mapMaybe tryFlype $ allDarts tangle
	where
		tryFlype ab = do
			let ba = opposite ab
			let ac = nextCCW ab
			let ca = opposite ac
			when (isLeg ba || isLeg ca) Nothing

			let a = incidentCrossing ab
			let b = incidentCrossing ba
			let c = incidentCrossing ca
			when (b == c || a == b || a == c) Nothing

			let ae = nextCCW ac
			let ad = nextCW ab

			((rp, sq), sub) <-
				restingPart tangle [ba, ca] >>= \ (lst, s) ->
					case lst of
						[x, y] -> Just $! ((x, y), s)
						_      -> Nothing

			return $! moveZ tangle $ do
				substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
				connectC [(rp, ae), (sq, ad)]
				flipC $ filter ((sub !) . crossingIndex) $ allCrossings tangle
