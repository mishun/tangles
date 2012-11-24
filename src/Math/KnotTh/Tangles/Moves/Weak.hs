module Math.KnotTh.Tangles.Moves.Weak
	( neighbours
	) where

import Data.Maybe
import Control.Monad (guard)
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move


neighbours :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighbours tangle = concatMap (\ f -> f tangle) [neighboursBorderCrossing, neighboursBorderLoop]


neighboursBorderCrossing :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighboursBorderCrossing tangle = mapMaybe tryReduceLeg $ allLegs tangle
	where
		tryReduceLeg xa = do
			let ax = opposite xa
			guard $ isDart ax

			let ay = nextCCW ax
			let ya = nextCCW xa
			guard $ ya == opposite ay

			let a = incidentCrossing ax

			let ap = nextCCW ay
			let aq = nextCCW ap

			let pa = opposite ap
			let qa = opposite aq

			return $! moveZ tangle $ do
				maskC [a]
				if qa == ap
					then connectC [(xa, ya)] >> emitCircle
					else connectC [(pa, ya), (qa, xa)]


neighboursBorderLoop :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighboursBorderLoop tangle = mapMaybe tryReduceLoop $ allLegs tangle
	where
		tryReduceLoop xa = do
			let ax = opposite xa
			guard $ isDart ax

			let abr = nextCCW ax
			let abl = nextCCW abr
			let ap = nextCW ax

			let bar = opposite abr
			guard $ isDart bar

			let bal = nextCW bar
			let by = nextCCW bar
			let bq = nextCCW by

			let yb = nextCCW xa

			guard $ yb == opposite by
			guard $ abl == opposite bal
			guard $ passOver ax /= passOver by

			return $! moveZ tangle $ do
				substituteC [(abl, ap), (bal, bq)]
				connectC [(ax, by), (ap, xa), (bq, yb)]
