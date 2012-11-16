module Math.KnotTh.Tangles.Paths
	( containingDirectedPath
	, containingUndirectedPath
	, directedPathsDecomposition
	, undirectedPathsDecomposition
	, containingThread
	, allThreads
	, containingFaceLeft
	, containingFaceRight
	, allFaces
	) where

import qualified Data.Set as Set
import Data.List (foldl')
import Math.KnotTh.Tangles


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


containingThread :: Dart ct -> [(Dart ct, Dart ct)]
containingThread = containingUndirectedPath continuation


allThreads :: Tangle ct -> [[(Dart ct, Dart ct)]]
allThreads = undirectedPathsDecomposition continuation


containingFaceLeft :: Dart ct -> [Dart ct]
containingFaceLeft = containingDirectedPath (nextCW, nextCCW)


containingFaceRight :: Dart ct -> [Dart ct]
containingFaceRight = containingDirectedPath (nextCCW, nextCW)


allFaces :: Tangle ct -> [[Dart ct]]
allFaces = directedPathsDecomposition (nextCW, nextCCW)
