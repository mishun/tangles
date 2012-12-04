module Math.KnotTh.Invariants.Skein.JonesPolynomial
	( ofTangle
	) where

import Data.List (sort)
import Data.Array.Unboxed (UArray, array, (!))
import qualified Data.IntMap as Map
import Math.Algebra.Group.Dn (fromReflectionRotation)
import Math.KnotTh.Tangles.NonAlternating


ofTangle :: (NonAlternatingTangle, Int) -> [(Scheme, Poly)]
ofTangle = jonesPolynomial


type Poly = [(Int, Int)]


normalize :: [(Int, Int)] -> Poly
normalize list = sort $ filter ((/= 0) . snd) $ Map.assocs res
	where
		update new prev = case prev of
			Nothing  -> Just new
			Just !pv -> Just $! (pv + new)

		res = foldl (\ m (p, e) -> Map.alter (update e) p m) Map.empty list


negative :: Poly -> Poly
negative = normalize . map (\ (p, e) -> (p, -e))


(<+>), (<*>) :: Poly -> Poly -> Poly
(<+>) l r = normalize (l ++ r)
(<*>) l r = normalize [ (ap + bp, ae * be) | (ap, ae) <- l, (bp, be) <- r ]


power :: (Integral ord) => ord -> Poly -> Poly
power p x
	| p == 0    = []
	| p < 0     = error "power for ring must be non-negative"
	| otherwise = mul p [(0, 1)] x
	where
		mul 0 acc _ = acc
		mul n acc sq = mul half newAcc (sq <*> sq)
			where
				(half, rest) = divMod n 2
				newAcc
					| rest == 1  = sq <*> acc
					| otherwise  = acc

type Scheme = [(Int, Int)]


u, a, b, d :: Poly
u = [(0, 1)]
a = [(1, 1)]
b = [(-1, 1)]
d = [(-2, -1), (2, -1)]


jonesPolynomial :: (NonAlternatingTangle, Int) -> [(Scheme, Poly)]
jonesPolynomial (tangle, circles)
	| l == 0     = bruteForceJones (tangle, circles)
	| otherwise  =
		minimum $ map bruteForceJones $ do
			refl <- [False, True]
			rot <- [0 .. l - 1]
			let g = fromReflectionRotation l (refl, rot)
			let t = transformTangle g tangle
			[(t, circles), (invertCrossings t, circles)]
	where
		l = numberOfLegs tangle


bruteForceJones :: (NonAlternatingTangle, Int) -> [(Scheme, Poly)]
bruteForceJones (tangle, circles) = map (\ (sch, poly) -> (sch, wm <*> cm <*> poly)) $ skein (allCrossings tangle) [] u
	where
		cm = power circles d

		wm =	let w = selfWrithe tangle
			in power ((* 3) $ abs w) (negative $ if w >= 0 then b else a)

		skein [] assocs mul =
			let (sch, poly) = reductionOutcome tangle (array (1, numberOfCrossings tangle) assocs)
			in [(sch, mul <*> poly)]

		skein (c : rest) assocs mul = merge sa sb
			where
				sa = skein rest ((crossingIndex c, False) : assocs) (a <*> mul)
				sb = skein rest ((crossingIndex c, True) : assocs) (b <*> mul)

				merge [] bl = bl
				merge al [] = al
				merge ((as, ap) : al) ((bs, bp) : bl) =
					case compare as bs of
						LT -> (as, ap) : merge al ((bs, bp) : bl)
						GT -> (bs, bp) : merge ((as, ap) : al) bl
						EQ ->	let s = ap <+> bp
							in if s == []
								then merge al bl
								else (as, s) : merge al bl


reductionOutcome :: NonAlternatingTangle -> UArray Int Bool -> (Scheme, Poly)
reductionOutcome tangle reduction = (scheme, power circles d)
	where
		circles = (length paths) - (length pairs)

		scheme = sort $ map toPositionPair pairs
			where
				toPositionPair (al, bl) = (min ap bp, max ap bp)
					where
						ap = legPlace al
						bp = legPlace bl

		pairs = map (\ path -> (fst $ head path, snd $ last path)) $ filter (isLeg . fst . head) paths

		paths = undirectedPathsDecomposition smooting tangle
			where
				smooting drt =
					if (passOver drt) == (reduction ! crossingIndex (incidentCrossing drt))
						then nextCCW drt
						else nextCW drt
