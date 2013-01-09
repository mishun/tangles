module Math.KnotTh.Invariants.Skein.SkeinM.Reduction
	( isFinishedST
	, greedyReductionST
	) where

import Control.Monad.ST (ST)
import Control.Monad (unless)
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.State
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
			    	r <- h s v
			    	unless r $ tryReductions t

			tryReductions [ tryRelaxVertex ]
			greedyReductionST s
