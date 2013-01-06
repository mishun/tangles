module Math.KnotTh.Invariants.Skein.Applied
	( module Math.KnotTh.Invariants.Skein.Relation
	, module Math.KnotTh.Invariants.Skein.StateSum
	, module Math.KnotTh.Invariants.Skein.Knotted
	, evaluateSkeinRelation
	) where

import Control.Monad (void, forM)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.Knotted
import Math.KnotTh.Invariants.Skein.SkeinM


evaluateSkeinRelation :: (SkeinRelation rel a, SkeinResult a res k c d) => rel -> k ArbitraryCrossing -> res
evaluateSkeinRelation relation = runSkein relation $ \ vertices -> do
	p <- concat `fmap` forM vertices (\ v -> vertexDegree v >>= \ d -> return $! zip (repeat v) [0 {- .. -}, d - 1])

	let try [] = error "impossible"
	    try ((v, i) : t) = do
	    	(u, _) <- neighbour (v, i)
	    	if u /= v
	    		then contract (v, i)
	    		else try t

	void $ try p
