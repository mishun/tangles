module Math.KnotTh.Link.Table.Access
    ( numberOfLinks
    , link
    , numberOfKnots
    , knot
    ) where

import qualified Data.Map as M
import Text.Printf
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Link.GaussCode
import Math.KnotTh.Link.Table.List


maxC :: Int
maxC = maximum $ map (fst . fst) $ map (\ (n, l) -> ((n, 1), l)) listOfKnotCodes ++ listOfLinkCodes


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
    | cross < 1                      = error $ printf "numberOfLinks: crossing number %i is non-positive" cross
    | comps < 1                      = error $ printf "numberOfLinks: components number %i is non-positive" comps
    | cross > maxC                   = error $ printf "numberOfLinks: table contains only links with <= %i crossings, but %i crossings requested" maxC cross
    | M.member (cross, comps) sizes  = sizes M.! (cross, comps)
    | otherwise                      = 0


link :: Int -> Int -> Int -> NonAlternatingLink
link comps cross number
    | number <= 0    = error $ printf "link: link number %i is non-positive" number
    | number > maxN  = error $ printf "link: link number %i is out of bound %i" number maxN
    | otherwise      = table M.! (cross, comps, number)
    where
        maxN = numberOfLinks comps cross


numberOfKnots :: Int -> Int
numberOfKnots = numberOfLinks 1


knot :: Int -> Int -> NonAlternatingLink
knot = link 1
