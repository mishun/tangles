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
import Math.KnotTh.Enumeration.DiagramInfo


data SiftState k v = St
	{ set  :: !DS.IntDisjointSet
	, keys :: !(M.Map k Int)
	, vals :: !(IM.IntMap v)
	}


siftByEquivalenceClasses ::
	(Ord c, CrossingType ct, KnottedWithConnectivity knot crossing dart, DiagramInfo info)
		=> (knot ct -> c)
		-> [knot ct -> [knot ct]]
		-> (forall m. (Monad m) => (knot ct -> m ()) -> m ())
		-> [info (knot ct)]

siftByEquivalenceClasses isomorphismTest moves enumerateDiagrams =
	let final = flip execState (St { set = DS.empty, keys = M.empty, vals = IM.empty }) $ do
		let declareEquivalent !a !b = do
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
				let (Just !ai, !ds1) = DS.lookup (keys st M.! a) ds0
				let (Just !bi, !ds2) = DS.lookup (keys st M.! b) ds1
				let !ds3 = DS.union ai bi ds2
				let (Just !resi, !ds4) = DS.lookup ai ds3
				let !result = merge (vals st IM.! bi) $! vals st IM.! ai
				put $! st { set = ds4, vals = IM.insert resi result $! IM.delete bi $! IM.delete ai $! vals st }

		let insert !code !current = do
			!st <- get
			let ds = set st
			case M.lookup code $! keys st of
				Nothing   -> do
					let !rootId = DS.size ds
					put $! st { set = DS.insert rootId ds, keys = M.insert code rootId $! keys st, vals = IM.insert rootId current $! vals st }
					return True
				Just elId -> do
					let (Just rootId, ds') = DS.lookup elId ds
					put $! st { set = ds', vals = IM.insertWith' merge rootId current $! vals st }
					return False

		enumerateDiagrams $
			fix (\ dfs !prevCode !diagram -> do
				let code = isomorphismTest diagram
				inserted <- insert code $ wrap diagram
				maybe (return ()) (declareEquivalent code) prevCode
				when inserted $
					forM_ (concatMap ($ diagram) moves) (dfs $! Just code)
				) Nothing
	in IM.elems $ vals final
