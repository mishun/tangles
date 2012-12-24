module Math.KnotTh.Link.LinkTable
	( numberOfLinks
	, link
	, numberOfKnots
	, knot
	, singleCrossingUnknot
	, hopfLink
	, leftTrefoilKnot
	, rightTrefoilKnot
	, figureEightKnot
	, leftCinquefoilKnot
	, rightCinquefoilKnot
	) where

import qualified Data.Map as M
import Text.Printf (printf)
import Math.KnotTh.Link.NonAlternating (NonAlternatingLink, invertCrossings)
import Math.KnotTh.Link.GaussCode
import Math.KnotTh.Link.GaussCodesList


maxN :: Int
maxN = maximum $ map (fst . fst) gaussCodes


table :: M.Map (Int, Int, Int) NonAlternatingLink
table = M.fromList $ do
	((cross, comps), list) <- gaussCodes
	(code, number) <- zip list [1 ..]
	return $! ((cross, comps, number), fromGaussCode code)


sizes :: M.Map (Int, Int) Int
sizes = M.fromList $ do
	(k, l) <- gaussCodes
	return $! (k, length l)


numberOfLinks :: Int -> Int -> Int
numberOfLinks comps cross
	| cross < 1                      = error "crossing number is non-positive"
	| comps < 1                      = error "components number is non-positive"
	| cross > maxN                   = error $ printf "table contains only links with <= %i crossings" maxN
	| M.member (cross, comps) sizes  = sizes M.! (cross, comps)
	| otherwise                      = 0


link :: Int -> Int -> Int -> NonAlternatingLink
link comps cross number
	| number <= 0                         = error "link number is non-positive"
	| number > numberOfLinks comps cross  = error "link number is out of bound"
	| otherwise                           = table M.! (cross, comps, number)


numberOfKnots :: Int -> Int
numberOfKnots = numberOfLinks 1


knot :: Int -> Int -> NonAlternatingLink
knot = link 1


singleCrossingUnknot :: NonAlternatingLink
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: NonAlternatingLink
hopfLink = link 1 2 1


leftTrefoilKnot :: NonAlternatingLink
leftTrefoilKnot = knot 3 1


rightTrefoilKnot :: NonAlternatingLink
rightTrefoilKnot = invertCrossings leftTrefoilKnot


figureEightKnot :: NonAlternatingLink
figureEightKnot = knot 4 1


leftCinquefoilKnot :: NonAlternatingLink
leftCinquefoilKnot = knot 5 1


rightCinquefoilKnot :: NonAlternatingLink
rightCinquefoilKnot = invertCrossings leftCinquefoilKnot
