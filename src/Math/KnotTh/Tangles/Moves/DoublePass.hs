module Math.KnotTh.Tangles.Moves.DoublePass
	( neighbours
	) where

import Data.Maybe
import Debug.Trace
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Resting


neighbours :: Tangle ArbitraryCrossing -> [(Tangle ArbitraryCrossing, Int)]
neighbours tangle = mapMaybe tryDoublePass $ allDarts tangle
	where
		tryDoublePass ab
			| unexpectedLeg           = Nothing
			| any isLeg incomingA     = Nothing
			| any isLeg incomingB     = Nothing
			| isNothing maybeA        = Nothing
			| isNothing maybeB        = Nothing
		--	| length outcomingA /= 2  = Nothing
		--	| length outcomingB /= 2  = Nothing
			| otherwise               = trace "found" Nothing
			where
				ba = opposite ab
				bc = nextCCW ba
				cb = opposite bc
				cd = nextCW cb
				dc = opposite cd

				unexpectedLeg = isLeg ba || isLeg cb || isLeg dc

				incomingA = map opposite [continuation ab, nextCW ab, nextCCW cb, continuation cb]
				incomingB = map opposite [continuation dc, nextCW dc, nextCCW bc, continuation bc]

				maybeA = restingPart tangle incomingA
				maybeB = restingPart tangle incomingB

				outcomingA = fst $ fromJust maybeA
				outcomingB = fst $ fromJust maybeB
