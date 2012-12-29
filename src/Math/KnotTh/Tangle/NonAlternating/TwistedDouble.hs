module Math.KnotTh.Tangle.NonAlternating.TwistedDouble
	( twistedDouble
	) where

import Data.Bits ((.&.))
import Data.Array.Base (listArray, (!))
import Data.Array.Unboxed (UArray)
import Math.KnotTh.Tangle.NonAlternating


twistedDouble :: NonAlternatingTangle -> NonAlternatingTangle
twistedDouble tangle = implode (2 * numberOfFreeLoops tangle, border, conn)
	where
		w = selfWritheArray tangle

		offset :: UArray Int Int
		offset =
			let n = numberOfCrossings tangle
			in listArray (1, n) $ scanl (\ !p !i -> p + if (w ! i) == 0 then 4 else 6) 1 [1 .. n]

		f i a
			| isLeg b    =
				let p = legPlace b
				in (0, 2 * p + i)
			| otherwise  =
				let c = crossingIndex $ incidentCrossing b
				    p = dartPlace b
				in ((offset ! c) + ((p + 1 - i) .&. 3), p)
			where
				b = opposite a

		border = concatMap (\ l -> [f 1 l, f 0 l]) $ allLegs tangle

		conn = do
			cross <- allCrossings tangle
			let c = crossingIndex cross
			let s = crossingState cross
			let [d0, d1, d2, d3] = incidentDarts cross
			let c0 = offset ! c ; c1 = c0 + 1 ; c2 = c1 + 1 ; c3 = c2 + 1
			if (w ! c) == 0
				then
					[ ([f 0 d0 , (c1, 3), (c3, 0), f 1 d3 ], s)
					, ([f 1 d0 , f 0 d1 , (c2, 0), (c0, 1)], s)
					, ([(c1, 2), f 1 d1 , f 0 d2 , (c3, 1)], s)
					, ([(c0, 2), (c2, 3), f 1 d2 , f 0 d3 ], s)
					]
				else
					let c4 = c3 + 1 ; c5 = c4 + 1
					    r | (w ! c) > 0  = underCrossing
					      | otherwise    = overCrossing
					in
						[ ([f 0 d0 , (c1, 3), (c4, 0), f 1 d3 ], s)
						, ([f 1 d0 , f 0 d1 , (c4, 1), (c0, 1)], s)
						, ([(c5, 2), f 1 d1 , f 0 d2 , (c3, 1)], s)
						, ([(c5, 3), (c2, 3), f 1 d2 , f 0 d3 ], s)
						, ([(c0, 2), (c1, 2), (c5, 1), (c5, 0)], r)
						, ([(c4, 3), (c4, 2), (c2, 0), (c3, 0)], r)
						]
