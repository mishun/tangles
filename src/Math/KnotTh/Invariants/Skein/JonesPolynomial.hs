module Math.KnotTh.Invariants.Skein.JonesPolynomial
	( fromTangle
	) where

import Math.KnotTh.Tangles.NonAlternating


fromTangle :: NonAlternatingTangle -> [Int]
fromTangle _ = []

{-
module Math.KnotTh.Tangles.Invariants.JonesPolynomial
	(
	  jonesPolynomial
	) where

import qualified Data.List as List
import qualified Data.Array as Array

import qualified Math.Algebra.Group as Group
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.KnotTh.Tangles.TangleSt as TangleSt

import Math.Algebra.Ring
import Math.Algebra.Polynomial
import Math.KnotTh
import Math.KnotTh.Crossings.ArbitraryCrossing
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Util.Arbitrary
import Math.KnotTh.Tangles.Util.Paths
import Math.KnotTh.Tangles.Util.Writhe


type Poly = Polynomial Z Integer Integer
type Scheme = [(Int, Int)]


u, a, b, d :: Poly
u = fromList Z [(0, 1)]
a = fromList Z [(1, 1)]
b = fromList Z [(-1, 1)]
d = fromList Z [(-2, -1), (2, -1)]


jonesPolynomial :: (Tangle t c d ArbitraryCrossing) => (t, Int) -> [(Scheme, Poly)]
jonesPolynomial (tangle, circles)
	| l > 0      = minimum $ map bruteForceJones $ zip (concatMap tr els) (repeat circles)
	| otherwise  = bruteForceJones (tangle, circles)
	where
		l = numberOfLegs tangle

		els = Group.groupElements (Dn.D l)

		tr e =	let ct = TangleSt.cloneTransform e tangle
			in [ct, invertCrossings ct]


bruteForceJones :: (Tangle t c d ArbitraryCrossing) => (t, Int) -> [(Scheme, Poly)]
bruteForceJones (tangle, circles) = map (\ (sch, poly) -> (sch, wm <*> cm <*> poly)) $ skein (allCrossings tangle) [] u
	where
		cm = power circles d

		wm =	let w = selfWrithe tangle
			in power ((* 3) $ abs w) (negative $ if w >= 0 then b else a)

		skein [] assocs mul =
			let (sch, poly) = reductionOutcome tangle (Array.array (crossingsRange tangle) assocs)
			in [(sch, mul <*> poly)]

		skein (c : rest) assocs mul = merge sa sb
			where
				sa = skein rest ((c, False) : assocs) (a <*> mul)
				sb = skein rest ((c, True) : assocs) (b <*> mul)

				merge [] bl = bl
				merge al [] = al
				merge ((as, ap) : al) ((bs, bp) : bl) =
					case compare as bs of
						LT -> (as, ap) : merge al ((bs, bp) : bl)
						GT -> (bs, bp) : merge ((as, ap) : al) bl
						EQ ->	let s = ap <+> bp
							in if s == (zero $ ring s)
								then merge al bl
								else (as, s) : merge al bl


reductionOutcome :: (Tangle t c d ArbitraryCrossing) => t -> Array.Array c Bool -> (Scheme, Poly)
reductionOutcome tangle reduction = (scheme, power circles d)
	where
		circles = (length paths) - (length pairs)

		scheme = sort $ map toPositionPair pairs
			where
				toPositionPair (al, bl) = (min ap bp, max ap bp)
					where
						ap = legPosition al
						bp = legPosition bl

		pairs = map (\ path -> (fst $ head path, snd $ last path)) $ filter (isLeg . fst . head) paths

		paths = undirectedPathsDecomposition smooting tangle
			where
				smooting drt =
					if (passOver drt) == (reduction Array.! (incidentCrossing drt))
						then nextCCW drt
						else nextCW drt
-}
