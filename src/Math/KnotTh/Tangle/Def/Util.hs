module Math.KnotTh.Tangle.Def.Util
	( firstLeg
	, allLegOpposites
	, isAdjacentToBorder
	, maybeIncidentCrossing
	, maybeAdjacentCrossing
	, allLegsAndDarts
	, undirectedPathsDecomposition
	, allThreads
	) where

import Data.List (nub, sort, foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Math.KnotTh.Tangle.Def.Tangle


{-# INLINE firstLeg #-}
firstLeg :: Tangle ct -> Dart ct
firstLeg t = nthLeg t 0


{-# INLINE allLegOpposites #-}
allLegOpposites :: Tangle ct -> [Dart ct]
allLegOpposites = map opposite . allLegs


{-# INLINE isAdjacentToBorder #-}
isAdjacentToBorder :: Dart ct -> Bool
isAdjacentToBorder = isLeg . opposite


{-# INLINE maybeIncidentCrossing #-}
maybeIncidentCrossing :: Dart ct -> Maybe (Crossing ct)
maybeIncidentCrossing d
	| isLeg d    = Nothing
	| otherwise  = Just $! incidentCrossing d


{-# INLINE maybeAdjacentCrossing #-}
maybeAdjacentCrossing :: Dart ct -> Maybe (Crossing ct)
maybeAdjacentCrossing = maybeIncidentCrossing . opposite


{-# INLINE allLegsAndDarts #-}
allLegsAndDarts :: Tangle ct -> [Dart ct]
allLegsAndDarts tangle = allLegs tangle ++ allDarts tangle



containingDirectedPath :: (Dart ct -> Dart ct, Dart ct -> Dart ct) -> Dart ct -> [Dart ct]
containingDirectedPath (adjForward, adjBackward) start
	| isCycle    = forward
	| otherwise  = walkBackward (start, forward)
	where
		(forward, isCycle) = walkForward start

		walkForward d
			| isLeg opp     = ([d], False)
			| start == nxt  = ([d], True)
			| otherwise     = (d : nextPath, nextCycle)
			where
				opp = opposite d
				nxt = adjForward opp
				(nextPath, nextCycle) = walkForward nxt

		walkBackward (d, path)
			| isLeg d    = path
			| otherwise  = let prev = opposite $ adjBackward d in walkBackward (prev, prev : path)


containingUndirectedPath :: (Dart ct -> Dart ct) -> Dart ct -> [(Dart ct, Dart ct)]
containingUndirectedPath cont = map (\ d -> (d, opposite d)) . containingDirectedPath (cont, cont)


directedPathsDecomposition :: (Dart ct -> Dart ct, Dart ct -> Dart ct) -> Tangle ct -> [[Dart ct]]
directedPathsDecomposition continue = fst . foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (paths, s) d
			| Set.member d s  = (paths, s)
			| otherwise       = (path : paths, nextS)
			where
				path = containingDirectedPath continue d
				nextS = foldl' (\ curs a -> Set.insert a curs) s path


undirectedPathsDecomposition :: (Dart ct -> Dart ct) -> Tangle ct -> [[(Dart ct, Dart ct)]]
undirectedPathsDecomposition continue = fst . foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (!paths, s) d
			| Set.member d s  = (paths, s)
			| otherwise       = (path : paths, nextS)
			where
				path = containingUndirectedPath continue d
				nextS = foldl' (\ curs (a, b) -> Set.insert b $ Set.insert a curs) s path


--containingThread :: Dart ct -> [(Dart ct, Dart ct)]
--containingThread = containingUndirectedPath continuation


allThreads :: Tangle ct -> [[(Dart ct, Dart ct)]]
allThreads = undirectedPathsDecomposition continuation

{-
containingFaceLeft :: Dart ct -> [Dart ct]
containingFaceLeft = containingDirectedPath (nextCW, nextCCW)


containingFaceRight :: Dart ct -> [Dart ct]
containingFaceRight = containingDirectedPath (nextCCW, nextCW)
-}

allTangleFaces :: Tangle ct -> [[Dart ct]]
allTangleFaces = directedPathsDecomposition (nextCW, nextCCW)


instance KnottedWithConnectivity Tangle Crossing Dart where
	isConnected tangle
		| numberOfFreeLoops tangle /= 0  = False
		| otherwise                      = all (\ (a, b) -> Set.member a con && Set.member b con) edges
		where
			edges = allEdges tangle
			con = dfs (Set.empty) $ fst $ head edges
			dfs vis c
				| Set.member c vis  = vis
				| otherwise         = foldl' dfs (Set.insert c vis) neigh
				where
					neigh
						| isLeg c    = [opposite c]
						| otherwise  = [opposite c, nextCCW c, nextCW c]

	isPrime tangle = connections == nub connections
		where
			idm =	let faces = allTangleFaces tangle
				in Map.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

			connections = sort $ map getPair $ allEdges tangle
				where
					getPair (da, db) = (min a b, max a b)
						where
							a = idm Map.! da
							b = idm Map.! db
