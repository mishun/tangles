module Math.KnotTh.Invariants.Skein.Reduction
	( vertexDegree
	, evaluateSkeinRelation
	) where

import Control.Monad (void, forM)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.State


evaluateSkeinRelation :: (SkeinRelation r a, SkeinKnotted k c d) => r -> k ArbitraryCrossing -> a
evaluateSkeinRelation relation = runSkein relation $ \ vertices -> do
	p <- concat `fmap` forM vertices (\ v -> vertexDegree v >>= \ d -> return $! zip (repeat v) [0 .. d - 1])

	let try [] = error "impossible"
	    try ((v, i) : t) = do
	    	(u, j) <- neighbour (v, i)
	    	if u /= v
	    		then contract (v, i)
	    		else do
	    		    d <- vertexDegree v
	    		    if j == (i + 1) `mod` d
	    		    	then contract (v, i)
	    		    	else try t

	void $ try p
