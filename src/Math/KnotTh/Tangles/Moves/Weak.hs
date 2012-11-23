module Math.KnotTh.Tangles.Moves.Weak
	( neighbours
	) where

import Data.Maybe
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move


neighbours :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighbours tangle = concatMap (\ f -> f tangle) [neighboursBorderCrossing, neighboursBorderLoop]


neighboursBorderCrossing :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighboursBorderCrossing tangle = mapMaybe tryReduceLeg $ allLegs tangle
	where
		tryReduceLeg xa
			| isLeg ax           = Nothing
			| ya /= opposite ay  = Nothing
			| otherwise          =
				Just $ moveZ tangle $ do
					maskC [a]
					if qa == ap
						then do
							connectC [(xa, ya)]
							emitCircle
						else connectC [(pa, ya), (qa, xa)]
			where
				ax = opposite xa

				ay = nextCCW ax
				ya = nextCCW xa

				a = incidentCrossing ax

				ap = nextCCW ay
				aq = nextCCW ap

				pa = opposite ap
				qa = opposite aq


neighboursBorderLoop :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighboursBorderLoop tangle = mapMaybe tryReduceLoop $ allLegs tangle
	where
		tryReduceLoop xa
			| isLeg ax                    = Nothing
			| isLeg bar                   = Nothing
			| yb /= opposite by           = Nothing
			| abl /= opposite bal         = Nothing
			| passOver ax == passOver by  = Nothing
			| otherwise                   =
				Just $ moveZ tangle $ do
					substituteC [(abl, ap), (bal, bq)]
					connectC [(ax, by), (ap, xa), (bq, yb)]
			where
				ax = opposite xa
				abr = nextCCW ax
				abl = nextCCW abr
				ap = nextCW ax

				bar = opposite abr
				bal = nextCW bar
				by = nextCCW bar
				bq = nextCCW by

				yb = nextCCW xa
