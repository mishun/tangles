module Math.KnotTh.Invariants.KauffmanFPolynomial
	( kauffmanFPolynomial
	) where

import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LP
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Applied


type Poly = LP.LaurentMPoly Int


monomial :: Int -> String -> B.Q -> Poly
monomial a var d = LP.LP [(LP.LM $ M.fromList [(var, d)], a)]


data KauffmanFRelation a = KauffmanFRelation a a a a


instance (Ord a, Num a, Show a) => SkeinRelation (KauffmanFRelation a) a where
	circleFactor (KauffmanFRelation a a' _ z') = (a + a') * z' - 1

	initialLplus _ = InitialSum { ofLplus = 1, ofLzero = 0, ofLinfty = 0 }

	twistPFactor (KauffmanFRelation a _ _ _) = a

	twistNFactor (KauffmanFRelation _ a' _ _) = a'

	finalNormalization (KauffmanFRelation a a' _ _) knot =
		let factor =
			let w = selfWrithe knot
			in (if w <= 0 then a else a') ^ (abs w)
		in (factor *)


kauffmanFPolynomial :: (SkeinResult Poly r k c d) => k ArbitraryCrossing -> r
kauffmanFPolynomial = evaluateSkeinRelation $
	KauffmanFRelation
		(monomial 1 "a" 1) (monomial 1 "a" (-1))
		(monomial 1 "z" 1) (monomial 1 "z" (-1))
