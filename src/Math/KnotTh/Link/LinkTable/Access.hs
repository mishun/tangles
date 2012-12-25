module Math.KnotTh.Link.LinkTable.Access
	( numberOfLinks
	, link
	, numberOfKnots
	, knot
	) where

import qualified Data.Map as M
import Text.Printf (printf)
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Link.GaussCode
import Math.KnotTh.Link.LinkTable.List


maxN :: Int
maxN = maximum $ map (fst . fst) $ map (\ (n, l) -> ((n, 1), l)) listOfKnotCodes ++ listOfLinkCodes


table :: M.Map (Int, Int, Int) NonAlternatingLink
table = M.fromList $ do
	((cross, comps), list) <- map (\ (n, l) -> ((n, 1), l)) listOfKnotCodes ++ listOfLinkCodes
	(code, number) <- zip list [1 ..]
	return $! ((cross, comps, number), fromGaussCode code)


sizes :: M.Map (Int, Int) Int
sizes = M.fromList $ do
	(k, l) <- map (\ (n, l) -> ((n, 1), l)) listOfKnotCodes ++ listOfLinkCodes
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
