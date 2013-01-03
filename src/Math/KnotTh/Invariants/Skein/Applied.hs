module Math.KnotTh.Invariants.Skein.Applied
	( BracketLikeRelation(..)
	, jonesPolynomial
	, kauffmanXPolynomial
	, kauffmanFPolynomial
	) where

import qualified Data.Map as M
import Control.Monad (void, forM)
import qualified Math.Projects.KnotTheory.LaurentMPoly as P
import Math.KnotTh.Invariants.Skein.SkeinM


evaluateSkeinRelation :: (SkeinRelation r a, SkeinKnotted k c d) => r -> k ArbitraryCrossing -> a
evaluateSkeinRelation relation = runSkein relation $ \ vertices -> do
	p <- concat `fmap` forM vertices (\ v -> vertexDegree v >>= \ d -> return $! zip (repeat v) [0 {- .. -}, d - 1])

	let try [] = error "impossible"
	    try ((v, i) : t) = do
	    	(u, _) <- neighbour (v, i)
	    	if u /= v
	    		then contract (v, i)
	    		else try t

	void $ try p


data BracketLikeRelation a = BracketLikeRelation a a


instance (Eq a, Num a, Show a) => SkeinRelation (BracketLikeRelation a) a where
	circleFactor (BracketLikeRelation a b) = -(a * a + b * b)

	initialLplus (BracketLikeRelation a b) = InitialSum { ofLplus = 0, ofLzero = a, ofLinfty = b }

	twistPFactor = undefined

	twistNFactor = undefined

	finalNormalization (BracketLikeRelation a b) knot =
		let factor =
			let w = selfWrithe knot
			in (if w <= 0 then -a else -b) ^ abs (3 * w)
		in (factor *)


jonesPolynomial :: (SkeinKnotted k c d) => k ArbitraryCrossing -> P.LaurentMPoly Int
jonesPolynomial = evaluateSkeinRelation $
	let jonesVar = "t"
	in BracketLikeRelation
		(P.LP [(P.LM $ M.fromList [(jonesVar, -1 / 4)], 1)])
		(P.LP [(P.LM $ M.fromList [(jonesVar,  1 / 4)], 1)])


kauffmanXPolynomial :: (SkeinKnotted k c d) => k ArbitraryCrossing -> P.LaurentMPoly Int
kauffmanXPolynomial = evaluateSkeinRelation $
	let kauffmanXVar = "A"
	in BracketLikeRelation
		(P.LP [(P.LM $ M.fromList [(kauffmanXVar,  1)], 1)])
		(P.LP [(P.LM $ M.fromList [(kauffmanXVar, -1)], 1)])


data KauffmanFRelation a = KauffmanFRelation a a a a


instance (Eq a, Num a, Show a) => SkeinRelation (KauffmanFRelation a) a where
	circleFactor (KauffmanFRelation a a' _ z') = (a + a') * z' - 1

	initialLplus _ = InitialSum { ofLplus = 1, ofLzero = 0, ofLinfty = 0 }

	twistPFactor (KauffmanFRelation a _ _ _) = a

	twistNFactor (KauffmanFRelation _ a' _ _) = a'

	finalNormalization (KauffmanFRelation a a' _ _) knot =
		let factor =
			let w = selfWrithe knot
			in (if w <= 0 then a else a') ^ (abs w)
		in (factor *)


kauffmanFPolynomial :: (SkeinKnotted k c d) => k ArbitraryCrossing -> P.LaurentMPoly Int
kauffmanFPolynomial = evaluateSkeinRelation $
	KauffmanFRelation
		(P.LP [(P.LM $ M.fromList [("a",  1)], 1)])
		(P.LP [(P.LM $ M.fromList [("a", -1)], 1)])
		(P.LP [(P.LM $ M.fromList [("z",  1)], 1)])
		(P.LP [(P.LM $ M.fromList [("z", -1)], 1)])