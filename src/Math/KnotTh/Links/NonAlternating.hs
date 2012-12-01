module Math.KnotTh.Links.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Links
	, NonAlternatingLink
	, isAlternating
	) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Links


type NonAlternatingLink = Link ArbitraryCrossing


isAlternating :: NonAlternatingLink -> Bool
isAlternating = (== 0) . alternatingDefect


alternatingDefect :: NonAlternatingLink -> Int
alternatingDefect tangle =
	let defect a
		| passOver a == passOver b  = 1
		| otherwise                 = 0
		where
			b = opposite a
	in (sum $ map defect $ allDarts tangle) `div` 2
