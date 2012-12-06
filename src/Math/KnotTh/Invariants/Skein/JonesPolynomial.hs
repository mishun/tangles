module Math.KnotTh.Invariants.Skein.JonesPolynomial
	( jonesPolynomialOfLink
	, jonesPolynomialOfLink'
	, kauffmanXPolynomialOfLink
	, kauffmanXPolynomialOfLink'
	, ofTangle
	) where

import Data.List (sort)
import Data.Array.Unboxed (UArray, array, (!))
import Data.Array.ST (STUArray, newArray, getAssocs, readArray, writeArray)
import qualified Data.Map as Map
import Control.Monad.ST (ST, runST)
import qualified Math.Projects.KnotTheory.LaurentMPoly as LP
import Math.Algebra.Group.Dn (fromReflectionRotation)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Links.NonAlternating as L
import qualified Math.KnotTh.Tangles.NonAlternating as T


data Node a = Cross a a a a | Join a a deriving (Eq, Show, Read, Ord)

instance Functor Node where
	fmap f (Cross a b c d) = Cross (f a) (f b) (f c) (f d)
	fmap f (Join a b) = Join (f a) (f b)


kauffmanStateSums :: L.NonAlternatingLink -> [((Int, Int), Int)]
kauffmanStateSums link = runST $ do
	let n = numberOfCrossings link
	coeff <- newArray ((0, 0), (n, n + 1)) 0 :: ST s (STUArray s (Int, Int) Int)

	let kauffman !u !v list =
		case list of
			[]                          -> readArray coeff (u, v) >>= writeArray coeff (u, v) . (+ 1)
			Join a b : rest | a == b    -> kauffman u (v + 1) rest
			                | otherwise -> kauffman u v $! map (fmap $ \ x -> if x == a then b else x) rest
			Cross a b c d : rest        -> do
				kauffman u v $! Join a b : Join c d : rest
				kauffman (u + 1) v $! Join a d : Join b c : rest

	kauffman 0 0 $! flip map (allCrossings link) $ \ c ->
		let	label d = min (dartArrIndex d) (dartArrIndex $ opposite d)
			[d0, d1, d2, d3] = incidentDarts c
		in if passOver d0
			then Cross (label d0) (label d1) (label d2) (label d3)
			else Cross (label d1) (label d2) (label d3) (label d0)

	filter ((/= 0) . snd) `fmap` getAssocs coeff


kauffmanBracket :: (Num a) => (L.NonAlternatingLink -> Int) -> a -> a -> a -> (L.NonAlternatingLink, Int) -> a
kauffmanBracket calculateWrithe a b d (!link, !circles) = writheFactor * (b ^ numberOfCrossings link) * stateSum
	where
		writheFactor =
			let w = calculateWrithe link
			in (if w <= 0 then -a else -b) ^ abs (3 * w)

		stateSum = sum $ flip map (kauffmanStateSums link) $ \ ((u, v), k) ->
			fromIntegral k * (a ^ (u + u)) * (d ^ (v + circles - 1))


jonesPolynomialOfLink :: L.NonAlternatingLink -> LP.LaurentMPoly Int
jonesPolynomialOfLink link = jonesPolynomialOfLink' (link, 0)


jonesPolynomialOfLink' :: (L.NonAlternatingLink, Int) -> LP.LaurentMPoly Int
jonesPolynomialOfLink' = kauffmanBracket L.selfWrithe a b d
	where
		a = LP.LP [(LP.LM $ Map.fromList [("t", -1 / 4)], 1)]
		b = LP.LP [(LP.LM $ Map.fromList [("t",  1 / 4)], 1)]
		d = -((a * a) + (b * b))


kauffmanXPolynomialOfLink :: L.NonAlternatingLink -> LP.LaurentMPoly Int
kauffmanXPolynomialOfLink link = kauffmanXPolynomialOfLink' (link, 0)


kauffmanXPolynomialOfLink' :: (L.NonAlternatingLink, Int) -> LP.LaurentMPoly Int
kauffmanXPolynomialOfLink' = kauffmanBracket L.selfWrithe a b d
	where
		a = LP.LP [(LP.LM $ Map.fromList [("A",  1)], 1)]
		b = LP.LP [(LP.LM $ Map.fromList [("A", -1)], 1)]
		d = -((a * a) + (b * b))



ofTangle :: (T.NonAlternatingTangle, Int) -> [(Scheme, Poly)]
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


jonesPolynomial :: (T.NonAlternatingTangle, Int) -> [(Scheme, Poly)]
jonesPolynomial (tangle, circles)
	| l == 0     = bruteForceJones (tangle, circles)
	| otherwise  =
		minimum $ map bruteForceJones $ do
			refl <- [False, True]
			rot <- [0 .. l - 1]
			let g = fromReflectionRotation l (refl, rot)
			let t = T.transformTangle g tangle
			[(t, circles), (invertCrossings t, circles)]
	where
		l = T.numberOfLegs tangle


bruteForceJones :: (T.NonAlternatingTangle, Int) -> [(Scheme, Poly)]
bruteForceJones (tangle, circles) = map (\ (sch, poly) -> (sch, wm <*> cm <*> poly)) $ skein (allCrossings tangle) [] [(0, 1)]
	where
		cm = power circles d

		wm =	let w = T.selfWrithe tangle
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

		a = [(1, 1)]
		b = [(-1, 1)]
		d = [(-2, -1), (2, -1)]


reductionOutcome :: T.NonAlternatingTangle -> UArray Int Bool -> (Scheme, Poly)
reductionOutcome tangle reduction = (scheme, power circles d)
	where
		circles = (length paths) - (length pairs)

		scheme = sort $ map toPositionPair pairs
			where
				toPositionPair (al, bl) = (min ap bp, max ap bp)
					where
						ap = T.legPlace al
						bp = T.legPlace bl

		pairs = map (\ path -> (fst $ head path, snd $ last path)) $ filter (T.isLeg . fst . head) paths

		paths = T.undirectedPathsDecomposition smooting tangle
			where
				smooting drt =
					if (passOver drt) == (reduction ! crossingIndex (incidentCrossing drt))
						then nextCCW drt
						else nextCW drt

		d = [(-2, -1), (2, -1)]
