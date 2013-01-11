module Math.KnotTh.Invariants.Util.ThreadExpansion
	( threadExpansion
	) where

import Data.List (sort, elemIndex)
import Data.Array (array, (!))
import qualified Data.Set as Set
import Math.KnotTh.Tangle


threadExpansion :: (Ord inv, ThreadedCrossing ct) => (Tangle ct -> inv) -> Tangle ct -> [([Int], inv)]
threadExpansion invariant tangle =
	sort $ map processThreadSet $
		let subsets [] = [[]]
		    subsets (x : xl) = let nx = subsets xl in nx ++ map (x :) nx
		in subsets $ allThreads tangle
	where
		processThreadSet threads = (ecode, invariant threadTangle)
			where
				targetLegs = sort $ do
					t <- threads
					case t of
						[]                     -> []
						(a, _) : _ | isLeg a   -> [a, snd $ last t]
						           | otherwise -> []

				targets = sort $ snd $ foldl (foldl checkTarget) (Set.empty, []) threads
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
					| otherwise  = findTarget (threadContinuation v)
					where
						v = opposite u
						ix = indices ! crossingIndex (incidentCrossing v)

				threadTangle =
					let loops = length $ flip filter threads $ \ thread ->
						case thread of
							[]          -> True
							(x, _) : _  -> isDart x && all (\ (_, d) -> (indices ! crossingIndex (incidentCrossing d)) == 0) thread
					in implode (loops, map findTarget targetLegs, map (\ c -> (map findTarget $ incidentDarts c, crossingState c)) targets)

				ecode = sort $ map (\ t -> dist (fst $ head t) (snd $ last t)) $
					flip filter threads $ \ thread ->
						case thread of
							[]         -> False
							(d, _) : _ -> isLeg d
					where
						l = numberOfLegs tangle
						dist a b =
							let d = legPlace a - legPlace b
							in min (mod d l) (mod (-d) l)
