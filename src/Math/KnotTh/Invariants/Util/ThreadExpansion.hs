module Math.KnotTh.Invariants.Util.ThreadExpansion
	( threadExpansion
	) where

import Data.List (sort, elemIndex)
import Data.Array (array, (!))
import qualified Data.Set as Set
import Math.KnotTh.Tangle


threadExpansion :: (Ord inv, ThreadedCrossing ct) => (Tangle ct -> inv) -> Tangle ct -> [([Int], inv)]
threadExpansion invariant tangle = sort $ map (processThreadSet invariant tangle) sets
	where
		sets = subsets $ allThreads tangle
			where
				subsets [] = [[]]
				subsets (x : xl) =
					let nx = subsets xl
					in nx ++ map (x :) nx


processThreadSet :: (Ord inv, ThreadedCrossing ct) => (Tangle ct -> inv) -> Tangle ct -> [[(Dart ct, Dart ct)]] -> ([Int], inv)
processThreadSet invariant tangle threads = (ecode, invariant threadTangle)
	where
		targetLegs = sort $ do
			t <- threads
			let a = fst $ head t
			if isLeg a
				then [a, snd $ last t]
				else []

		targets = sort $ snd $ foldl (\ s t -> foldl checkTarget s t) (Set.empty, []) threads
			where
				checkTarget (s, lst) (_, b)
					| isLeg b         = (s, lst)
					| Set.member c s  = (s, c : lst)
					| otherwise       = (Set.insert c s, lst)
					where
						c = incidentCrossing b

		indices = array (crossingIndexRange tangle) $ map (\ (c, x) -> (crossingIndex c, x)) $ (zip (allCrossings tangle) (repeat 0)) ++ (zip targets [1 ..])

		findTarget u
			| isLeg v    =
				case elemIndex v targetLegs of
					Just i  -> (0, i)
					Nothing -> error "processThread: internal error"
			| ix > 0     = (ix, dartPlace v)
			| otherwise  = findTarget (continuation v)
			where
				v = opposite u
				ix = indices ! crossingIndex (incidentCrossing v)

		threadTangle = implode
			( length $ filter (all (\ (_, d) -> (indices ! crossingIndex (incidentCrossing d)) == 0)) $ filter (not . isLeg . fst . head) threads
			, map findTarget targetLegs
			, map (\ c -> (map findTarget $ incidentDarts c, crossingState c)) targets
			)

		ecode = sort $ map (\ t -> dist (fst $ head t) (snd $ last t)) $ filter (isLeg . fst . head) threads
			where
				l = numberOfLegs tangle
				dist a b =
					let d = legPlace a - legPlace b
					in min (mod d l) (mod (-d) l)