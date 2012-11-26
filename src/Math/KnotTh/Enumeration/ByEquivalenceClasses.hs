{-# LANGUAGE Rank2Types, ConstraintKinds #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses
	( siftByEquivalenceClasses
	) where

import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad.State.Strict (execState, get, put)
import Control.Monad (when, unless, forM_)
import qualified Data.IntDisjointSet as DS 
import Math.KnotTh.Knotted


data DiagramData x k = DiagramData
	{ representative     :: !k
	, numberOfCircles    :: !Int
	, representativeCode :: !x
	}


merge :: DiagramData x k -> DiagramData x k -> DiagramData x k
merge a _ = a


data SiftState x k = SiftState
	{ disjointSet     :: !DS.IntDisjointSet
	, codeIndices     :: !(Map.Map x Int)
	, representatives :: !(IntMap.IntMap (DiagramData x k))
	}


initialState :: (Ord x, CrossingType ct, Knotted k c d) => SiftState x (k ct)
initialState = SiftState
	{ disjointSet     = DS.empty
	, codeIndices     = Map.empty
	, representatives = IntMap.empty
	}


siftByEquivalenceClasses ::
	(Monad m, Ord code, CrossingType ct, Knotted knot crossing dart)
		=> (knot ct -> code)
		-> [knot ct -> [(knot ct, Int)]]
		-> (forall m'. (Monad m') => (knot ct -> m' ()) -> m' ())
		-> (knot ct -> m ())
		-> m ()

siftByEquivalenceClasses isomorphismTest moves enumerateDiagrams =
	let final = flip execState initialState $ do

		let areEquivalent a b = do
			!st <- get
			let ai = codeIndices st Map.! a
			let bi = codeIndices st Map.! b
			let (result, nextDS) = DS.equivalent ai bi (disjointSet st)
			put $! st { disjointSet = nextDS }
			return $! result

		let declareEquivalent a b = do
			eq <- areEquivalent a b
			unless eq $ do
				!st <- get
				let ds0 = disjointSet st
				let (Just ai, ds1) = DS.lookup (codeIndices st Map.! a) ds0
				let (Just bi, ds2) = DS.lookup (codeIndices st Map.! b) ds1
				let ad = representatives st IntMap.! ai
				let bd = representatives st IntMap.! bi
				let ds3 = DS.union ai bi ds2
				let (Just resi, ds4) = DS.lookup ai ds3
				put $! st
					{ disjointSet     = ds4
					, representatives = IntMap.insert resi (merge ad bd) $ IntMap.delete bi $ IntMap.delete ai $ representatives st
					}

		let insert current = do
			let code = representativeCode current
			!st <- get
			let ds = disjointSet st
			case Map.lookup code (codeIndices st) of
				Nothing   -> do
					let rootId = DS.size ds
					put $! st
						{ disjointSet     = DS.insert rootId ds
						, codeIndices     = Map.insert code rootId (codeIndices st)
						, representatives = IntMap.insert rootId current (representatives st)
						}
					return True

				Just elId -> do
					let (Just rootId, ds') = DS.lookup elId ds
					let nextData = merge (representatives st IntMap.! rootId) current
					put $! st { disjointSet = ds', representatives = IntMap.insert rootId nextData (representatives st) }
					return False

		let makeDiagramData (diagram, circles) = DiagramData
			{ representative     = diagram
			, numberOfCircles    = circles
			, representativeCode = isomorphismTest diagram
			}

		enumerateDiagrams $ \ !startDiagram ->
			flip fix (startDiagram, 0) $ \ dfs (diagram, circles) -> do
				let fatherCode = isomorphismTest diagram
				let current = makeDiagramData (diagram, circles)
				inserted <- insert current
				when inserted $ do
					forM_ [ (d, circles + c) | move <- moves, (d, c) <- move diagram ] $ \ (childDiagram, childCircles) -> do
						let childCode = isomorphismTest childDiagram
						when (childCircles == 0) $
							dfs (childDiagram, childCircles)
						declareEquivalent fatherCode childCode

		return []

	in forM_ (map representative $ IntMap.elems $ representatives final)
