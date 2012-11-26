module Math.KnotTh.Tangles.Connectivity
	( isConnected
	, isPrime
	) where

import Data.List (foldl', nub, sort)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Paths (allFaces)


isConnected :: Tangle ct -> Bool
isConnected tangle = all (\ (a, b) -> Set.member a con && Set.member b con) edges
	where
		edges = allEdges tangle

		con = dfs (Set.empty) $ fst $ head edges

		dfs vis c
			| Set.member c vis  = vis
			| otherwise         = foldl' dfs (Set.insert c vis) neigh
			where
				neigh = if isLeg c
					then [opposite c]
					else [opposite c, nextCCW c, nextCW c]


isPrime :: Tangle ct -> Bool
isPrime tangle = connections == nub connections
	where
		idm =	let faces = allFaces tangle
			in Map.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

		connections = sort $ map getPair $ allEdges tangle
			where
				getPair (da, db) = (min a b, max a b)
					where
						a = idm Map.! da
						b = idm Map.! db
