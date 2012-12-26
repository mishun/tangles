module Math.KnotTh.Tangle.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Tangle
	, NonAlternatingTangle
	, lonerOverCrossing
	, lonerUnderCrossing
	, isAlternating
	, alternatingDefect
	) where

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
