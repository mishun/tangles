module Math.KnotTh.Tangle.Moves.Weak
	( neighbours
	) where

import Data.Maybe
import Control.Monad (guard)
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move


neighbours :: NonAlternatingTangle -> [NonAlternatingTangle]
neighbours tangle = concatMap (\ f -> f tangle) [neighboursBorderCrossing, neighboursBorderLoop]


neighboursBorderCrossing :: NonAlternatingTangle -> [NonAlternatingTangle]
neighboursBorderCrossing tangle = mapMaybe tryReduceLeg $ allLegs tangle
	where
		tryReduceLeg xa = do
			let ax = opposite xa
			guard $ isDart ax

			let ay = nextCCW ax
			    ya = nextCCW xa

			guard $ ya == opposite ay

			let a = incidentCrossing ax
			    ap = nextCCW ay
			    aq = nextCCW ap
			    pa = opposite ap
			    qa = opposite aq

			return $! move tangle $ do
				maskC [a]
				if qa == ap
					then connectC [(xa, ya)] >> emitCircle
					else connectC [(pa, ya), (qa, xa)]


neighboursBorderLoop :: NonAlternatingTangle -> [NonAlternatingTangle]
neighboursBorderLoop tangle = mapMaybe tryReduceLoop $ allLegs tangle
	where
		tryReduceLoop xa = do
			let ax = opposite xa
			guard $ isDart ax

			let abr = nextCCW ax
			    abl = nextCCW abr
			    ap = nextCW ax
			    bar = opposite abr

			guard $ isDart bar

			let bal = nextCW bar
			    by = nextCCW bar
			    bq = nextCCW by
			    yb = nextCCW xa

			guard $ yb == opposite by
			guard $ abl == opposite bal
			guard $ passOver ax /= passOver by

			return $! move tangle $ do
				substituteC [(abl, ap), (bal, bq)]
				connectC [(ax, by), (ap, xa), (bq, yb)]
