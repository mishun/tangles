module Math.KnotTh.Tangles.Invariants.InvariantsSet
	(
	  invariantsSet
	) where

import Math.Algebra.Ring
import Math.Algebra.Polynomial
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Invariants.JonesPolynomial
import Math.KnotTh.Tangles.Invariants.ThreadExpansion
import Math.KnotTh.Tangles.Invariants.LinkingNumber


type Poly = Polynomial Z Integer Integer
type Scheme = [(Int, Int)]


invariantsSet :: (Tangle t c d ArbitraryCrossing) => t -> ([Int], [([Int], [(Scheme, Poly)])])
invariantsSet tangle = (linkingNumber tangle, threadExpansion jonesPolynomial tangle)
