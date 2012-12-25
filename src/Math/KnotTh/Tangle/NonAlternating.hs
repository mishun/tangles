module Math.KnotTh.Tangle.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Tangle
	, NonAlternatingTangle
	, lonerOverCrossing
	, lonerUnderCrossing
	, isAlternating
	, alternatingDefect
	, selfWrithe
	, doubling
	) where

import Data.Bits ((.&.))
import qualified Data.Map as Map
import Data.List (foldl')
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangle


type NonAlternatingTangle = Tangle ArbitraryCrossing


lonerOverCrossing :: NonAlternatingTangle
lonerOverCrossing = lonerTangle overCrossing


lonerUnderCrossing :: NonAlternatingTangle
lonerUnderCrossing = lonerTangle underCrossing


isAlternating :: NonAlternatingTangle -> Bool
isAlternating = (== 0) . alternatingDefect


alternatingDefect :: NonAlternatingTangle -> Int
alternatingDefect tangle =
	let defect a
		| isDart b && passOver a == passOver b  = 1
		| otherwise                             = 0
		where
			b = opposite a
	in (sum $ map defect $ allDarts tangle) `div` 2


selfWrithe :: NonAlternatingTangle -> Int
selfWrithe =
	let threadWrithe =
		let edgeWrithe (!w, !m) (!d, _)
			| isLeg d          = (w, m)
			| Map.member cr m  = (w + writhe (m Map.! cr) d, m)
			| otherwise        = (w, Map.insert cr d m)
			where
				cr = incidentCrossing d
		in fst . foldl' edgeWrithe (0, Map.empty)
	in sum . map threadWrithe . allThreads


doubling :: NonAlternatingTangle -> NonAlternatingTangle
doubling tangle = implode (2 * numberOfFreeLoops tangle, border, conn)
	where
		f i a
			| isLeg b    =
				let p = legPlace b
				in (0, 2 * p + i)
			| otherwise  =
				let c = crossingIndex $ incidentCrossing b
				    p = dartPlace b
				in (4 * c - 3 + (.&.) (p + 1 - i) 3, p)
			where
				b = opposite a

		border = concatMap (\ l -> [f 1 l, f 0 l]) $ allLegs tangle

		conn = do
			c <- allCrossings tangle
			let s = crossingState c
			let c0 = 4 * crossingIndex c - 3 ; c1 = c0 + 1 ; c2 = c1 + 1 ; c3 = c2 + 1
			let [d0, d1, d2, d3] = incidentDarts c
			id
				[ ([f 0 d0 , (c1, 3), (c3, 0), f 1 d3 ], s)
				, ([f 1 d0 , f 0 d1 , (c2, 0), (c0, 1)], s)
				, ([(c1, 2), f 1 d1 , f 0 d2 , (c3, 1)], s)
				, ([(c0, 2), (c2, 3), f 1 d2 , f 0 d3 ], s)
				]
