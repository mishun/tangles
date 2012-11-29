{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses
	( siftByEquivalenceClasses
	) where

import Data.Function (fix)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad.State.Strict (execState, get, put)
import Control.Monad (when, unless, forM_)
import qualified Data.IntDisjointSet as DS 
import Math.KnotTh.Knotted


data SiftState k v = St
	{ set  :: !DS.IntDisjointSet
	, keys :: !(M.Map k Int)
	, vals :: !(IM.IntMap v)
	}


siftByEquivalenceClasses ::
	(Ord c, CrossingType ct, Knotted knot crossing dart)
		=> (d -> d -> d)
		-> ((knot ct, Int) -> d)
		-> ((knot ct, Int) -> c)
		-> [knot ct -> [(knot ct, Int)]]
		-> (forall m. (Monad m) => (knot ct -> m ()) -> m ())
		-> [d]

siftByEquivalenceClasses merge wrap isomorphismTest moves enumerateDiagrams =
	let final = flip execState (St { set = DS.empty, keys = M.empty, vals = IM.empty }) $ do
		let declareEquivalent a b = do
			eq <- do
				!st <- get
				let ai = keys st M.! a
				let bi = keys st M.! b
				let (result, nextDS) = DS.equivalent ai bi $! set st
				put $! st { set = nextDS }
				return $! result
			unless eq $ do
				!st <- get
				let ds0 = set st
				let (Just ai, ds1) = DS.lookup (keys st M.! a) ds0
				let (Just bi, ds2) = DS.lookup (keys st M.! b) ds1
				let ad = vals st IM.! ai
				let bd = vals st IM.! bi
				let ds3 = DS.union ai bi ds2
				let (Just resi, ds4) = DS.lookup ai ds3
				put $! st { set = ds4, vals = IM.insert resi (merge ad bd) $ IM.delete bi $ IM.delete ai $ vals st }

		let insert code current = do
			!st <- get
			let ds = set st
			case M.lookup code $ keys st of
				Nothing   -> do
					let rootId = DS.size ds
					put $! st { set = DS.insert rootId ds, keys = M.insert code rootId $ keys st, vals = IM.insert rootId current $ vals st }
					return True
				Just elId -> do
					let (Just rootId, ds') = DS.lookup elId ds
					put $! st { set = ds', vals = IM.insertWith merge rootId current $ vals st }
					return False

		enumerateDiagrams $ \ !startDiagram ->
			fix (\ dfs !dc@(diagram, circles) !prevCode -> do
				let code = isomorphismTest dc
				inserted <- insert code $ wrap dc
				maybe (return ()) (declareEquivalent code) prevCode
				when inserted $
					forM_ [ (d, circles + c) | move <- moves, (d, c) <- move diagram ] $ \ child ->
						dfs child $! Just code
				) (startDiagram, 0) Nothing
		return ()
	in IM.elems $ vals final
