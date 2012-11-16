module Math.KnotTh.Tangles.Invariants.ThreadExpansion
	(
	  threadExpansion
	) where

import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Set as Set

import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.KnotTh
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.Paths


threadExpansion :: (Ord inv, Tangle t c d ct) => ((TangleSt.TangleSt ct, Int) -> inv) -> t -> [([Int], inv)]
threadExpansion invariant tangle = sort $ map (processThreadSet invariant tangle) sets
	where
		sets = subsets $ allThreads tangle
			where
				subsets [] = [[]]
				subsets (x : xl) =
					let nx = subsets xl
					in nx ++ map (x :) nx


processThreadSet :: (Ord inv, Tangle t c d ct) => ((TangleSt.TangleSt ct, Int) -> inv) -> t -> [[(d, d)]] -> ([Int], inv)
processThreadSet invariant tangle threads = (ecode, invariant (threadTangle, circles))
	where
		targetLegs = sort $ concatMap (\ t -> let a = fst $ head t in if isLeg a then [a, snd $ last t] else []) threads

		targets = sort $ snd $ foldl (\ s t -> foldl checkTarget s t) (Set.empty, []) threads
			where
				checkTarget (s, lst) (_, b)
					| isLeg b         = (s, lst)
					| Set.member c s  = (s, c : lst)
					| otherwise       = (Set.insert c s, lst)
					where
						c = incidentCrossing b

		indices = Array.array (crossingsRange tangle) $ (zip (allCrossings tangle) (repeat 0)) ++ (zip targets [1 ..])

		findTarget u
			| isLeg v    =
				case elemIndex v targetLegs of
					Just i  -> (0, i)
					Nothing -> error "processThread: internal error"
			| ix > 0     = (ix, place)
			| otherwise  = findTarget (continuation v)
			where
				v = opposite u

				(c, place) = begin v

				ix = (Array.!) indices c

		threadTangle = TangleSt.constructFromList (conn, sts)
			where
				conn = (map findTarget targetLegs) : map (map findTarget . incidentDarts) targets

				sts = map state targets

		circles = length $ filter (all (\ (_, d) -> (indices Array.! (incidentCrossing d)) == 0)) $ filter (not . isLeg . fst . head) threads

		ecode = sort $ map (\ t -> dist (fst $ head t) (snd $ last t)) $ filter (isLeg . fst . head) threads
			where
				l = numberOfLegs tangle

				dist a b =
					let d = legPosition a - legPosition b
					in min (mod (l + d) l) (mod (l - d) l)
