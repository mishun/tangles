module Math.KnotTh.Invariants.Skein.Applied
	( BracketLikeRelation(..)
	, jonesPolynomial
	, kauffmanXPolynomial
	) where

import qualified Data.Map as M
import qualified Math.Projects.KnotTheory.LaurentMPoly as P
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.Reduction


data BracketLikeRelation a = BracketLikeRelation a a


instance (Eq a, Num a) => SkeinRelation (BracketLikeRelation a) a where
	circleMultiple (BracketLikeRelation a b) = -(a * a + b * b)

	initialLplus (BracketLikeRelation a b) = InitialSum { ofLplus = 0, ofLzero = a, ofLinfty = b }

	finalNormalization r@(BracketLikeRelation a b) knot =
		let factor = loopsFactor * writheFactor
			where
				loopsFactor = circleMultiple r ^ numberOfFreeLoops knot
		   		writheFactor =
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
