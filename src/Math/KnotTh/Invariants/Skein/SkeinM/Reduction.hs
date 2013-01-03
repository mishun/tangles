module Math.KnotTh.Invariants.Skein.SkeinM.Reduction
	( isFinishedST
	, greedyReductionST
	) where

import Data.Array.MArray (readArray)
import Control.Monad.ST (ST)
import Control.Monad (unless)
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.Def
import Math.KnotTh.Invariants.Skein.SkeinM.Basic
import Math.KnotTh.Invariants.Skein.SkeinM.RelaxVertex


isFinishedST :: SkeinState s r a -> ST s Bool
isFinishedST s = do
	n <- numberOfAliveVerticesST s
	return $! n == 0


greedyReductionST :: (SkeinRelation r a) => SkeinState s r a -> ST s ()
greedyReductionST s = do
	mv <- dequeueST s
	case mv of
		Nothing -> return ()
		Just v  -> do
			let tryReductions [] = return ()
			    tryReductions (h : t) = do
			    	r <- h v s
			    	unless r $ tryReductions t
			tryReductions reductions
			greedyReductionST s


reductions :: (SkeinRelation r a) => [Int -> SkeinState s r a -> ST s Bool]
reductions =
	[ \ v s -> do
		degree <- vertexDegreeST v s
		if degree > 0
			then return False
			else do
				stateSum <- readArray (state s) v
				case stateSum of
					[]                 -> appendMultipleST 0 s
					[StateSummand _ x] -> appendMultipleST x s
					_                  -> fail "internal error: zero degree vertex and StateSum with length > 1"
				killVertexST v s
				return True

	, \ v s -> do
		degree <- vertexDegreeST v s

		let findLoop [] = return False
		    findLoop (i : t) = do
		    	(u, j) <- neighbourST (v, i) s
		    	if u /= v || j /= (i + 1) `mod` degree
		    		then findLoop t
		    		else do
		    			contractLoopST (v, i) s
		    			enqueueST v s
		    			return True

		findLoop [0 .. degree - 1]
	]
