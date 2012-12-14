module Math.KnotTh.Link.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Link
	, NonAlternatingLink
	, isAlternating
	, alternatingDefect
	, selfWrithe
	, fromGaussCode
	, toGaussCode
	, fromDTCode
	, toDTCode
	, singleCrossingUnknot
	, hopfLink
	, leftTrefoilKnot
	, rightTrefoilKnot
	, figureEightKnot
	, leftCinquefoilKnot
	, rightCinquefoilKnot
	) where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Array.ST (STUArray, newArray_)
import Control.Monad.ST (ST, runST)
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


fromGaussCode :: [[Int]] -> NonAlternatingLink
fromGaussCode _ = runST $ do
	let n = undefined
	sign <- newArray_ (1, n) :: ST s (STUArray s Int Bool)
	return $! undefined


toGaussCode :: NonAlternatingLink -> [[Int]]
toGaussCode _ = undefined


fromDTCode :: [Int] -> NonAlternatingLink
fromDTCode _ = undefined


toDTCode :: NonAlternatingLink -> [Int]
toDTCode _ = undefined


singleCrossingUnknot :: NonAlternatingLink
singleCrossingUnknot = fromList 0 [([(1, 1), (1, 0), (1, 3), (1, 2)], overCrossing)]


hopfLink :: NonAlternatingLink
hopfLink = fromList 0
	[ ([(2, 1), (2, 0), (2, 3), (2, 2)], overCrossing)
	, ([(1, 1), (1, 0), (1, 3), (1, 2)], overCrossing)
	]


leftTrefoilKnot :: NonAlternatingLink
leftTrefoilKnot = fromList 0
	[ ([(3, 1), (2, 0), (2, 3), (3, 2)], overCrossing)
	, ([(1, 1), (3, 0), (3, 3), (1, 2)], overCrossing)
	, ([(2, 1), (1, 0), (1, 3), (2, 2)], overCrossing)
	]


rightTrefoilKnot :: NonAlternatingLink
rightTrefoilKnot = invertCrossings leftTrefoilKnot


figureEightKnot :: NonAlternatingLink
figureEightKnot = fromList 0
	[ ([(3, 1), (2, 0), (2, 3), (4, 2)], overCrossing)
	, ([(1, 1), (3, 0), (4, 3), (1, 2)], overCrossing)
	, ([(2, 1), (1, 0), (4, 1), (4, 0)], overCrossing)
	, ([(3, 3), (3, 2), (1, 3), (2, 2)], overCrossing)
	]


leftCinquefoilKnot :: NonAlternatingLink
leftCinquefoilKnot = fromList 0
	[ ([(5, 1), (2, 0), (2, 3), (5, 2)], overCrossing)
	, ([(1, 1), (3, 0), (3, 3), (1, 2)], overCrossing)
	, ([(2, 1), (4, 0), (4, 3), (2, 2)], overCrossing)
	, ([(3, 1), (5, 0), (5, 3), (3, 2)], overCrossing)
	, ([(4, 1), (1, 0), (1, 3), (4, 2)], overCrossing)
	]


rightCinquefoilKnot :: NonAlternatingLink
rightCinquefoilKnot = invertCrossings leftCinquefoilKnot
