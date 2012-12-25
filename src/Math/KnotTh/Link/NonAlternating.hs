module Math.KnotTh.Link.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Link
	, NonAlternatingLink
	, isAlternating
	, alternatingDefect
	, selfWrithe
	) where

import Data.List (foldl')
import qualified Data.Map as Map
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Link


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


selfWrithe :: NonAlternatingLink -> Int
selfWrithe =
	let threadWrithe =
		let edgeWrithe (!w, !m) (!d, _)
			| Map.member cr m  = (w + writhe (m Map.! cr) d, m)
			| otherwise        = (w, Map.insert cr d m)
			where
				cr = incidentCrossing d
		in fst . foldl' edgeWrithe (0, Map.empty)
	in sum . map threadWrithe . allThreads
