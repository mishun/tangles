module Math.KnotTh.Invariants.Skein.Reduction
	( vertexDegree
	, evaluateSkeinRelation
	) where

import Data.Function (fix)
import Control.Monad (void)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.State


evaluateSkeinRelation :: (SkeinRelation r a, SkeinKnotted k c d) => r -> k ArbitraryCrossing -> StateSum a
evaluateSkeinRelation relation = runSkein relation $
	fix $ \ continue -> do
		l <- aliveVertices
		case l of
			[v]   -> do
				d <- vertexDegree v
				if d == 0
					then return ()
					else do
						void $ contract (v, 0)
						continue
			v : _ -> do
				void $ contract (v, 0)
				continue
			_     -> fail "evaluateSkeinRelation: impossible"
