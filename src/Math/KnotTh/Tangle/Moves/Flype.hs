module Math.KnotTh.Tangle.Moves.Flype
	( neighbours
	) where

import Data.Maybe
import Data.Array.Unboxed ((!))
import Control.Monad (guard)
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move
import Math.KnotTh.Tangle.Moves.Resting


neighbours :: NonAlternatingTangle -> [NonAlternatingTangle]
neighbours tangle = mapMaybe tryFlype $ allDartsOfCrossings tangle
	where
		tryFlype ab = do
			let ba = opposite ab
			let ac = nextCCW ab
			let ca = opposite ac
			guard $ isDart ba && isDart ca

			let a = incidentCrossing ab
			let b = incidentCrossing ba
			let c = incidentCrossing ca
			guard $ b /= c && a /= b && a /= c

			let ae = nextCCW ac
			let ad = nextCW ab

			((rp, sq), sub) <-
				restingPart tangle [ba, ca] >>= \ (lst, s) ->
					case lst of
						[x, y] -> Just $! ((x, y), s)
						_      -> Nothing

			return $! move tangle $ do
				substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
				connectC [(rp, ae), (sq, ad)]
				flipC $ filter ((sub !) . crossingIndex) $ allCrossings tangle
