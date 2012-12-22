module Math.KnotTh.Invariants.Skein.JonesPolynomial
	( jonesPolynomialOfLink
	, kauffmanXPolynomialOfLink
	, jonesPolynomialOfTangle
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
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T


type Poly = LP.LaurentMPoly Int


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


kauffmanBracket :: (Num a) => (L.NonAlternatingLink -> Int) -> a -> a -> a -> L.NonAlternatingLink -> a
kauffmanBracket calculateWrithe a b d link = writheFactor * (b ^ numberOfCrossings link) * stateSum
	where
		writheFactor =
			let w = calculateWrithe link
			in (if w <= 0 then -a else -b) ^ abs (3 * w)

		stateSum = sum $ flip map (kauffmanStateSums link) $ \ ((u, v), k) ->
			fromIntegral k * (a ^ (u + u)) * (d ^ (v + numberOfFreeLoops link - 1))


jonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
jonesPolynomialOfLink = kauffmanBracket L.selfWrithe a b d
	where
		a = LP.LP [(LP.LM $ Map.fromList [("t", -1 / 4)], 1)]
		b = LP.LP [(LP.LM $ Map.fromList [("t",  1 / 4)], 1)]
		d = -((a * a) + (b * b))


kauffmanXPolynomialOfLink :: L.NonAlternatingLink -> Poly
kauffmanXPolynomialOfLink = kauffmanBracket L.selfWrithe a b d
	where
		a = LP.LP [(LP.LM $ Map.fromList [("A",  1)], 1)]
		b = LP.LP [(LP.LM $ Map.fromList [("A", -1)], 1)]
		d = -((a * a) + (b * b))


type Scheme = [(Int, Int)]


jonesPolynomialOfTangle :: T.NonAlternatingTangle -> [(Scheme, Poly)]
jonesPolynomialOfTangle tangle
	| l == 0     = bruteForceJones tangle
	| otherwise  =
		minimum $ map bruteForceJones $ do
			refl <- [False, True]
			rot <- [0 .. l - 1]
			let g = fromReflectionRotation l (refl, rot)
			let t = T.transformTangle g tangle
			[t, invertCrossings t]
	where
		l = T.numberOfLegs tangle


bruteForceJones :: T.NonAlternatingTangle -> [(Scheme, Poly)]
bruteForceJones tangle = map (\ (sch, poly) -> (sch, wm * cm * poly)) $ skein (allCrossings tangle) [] 1
	where
		cm = d ^ numberOfFreeLoops tangle

		wm =	let w = T.selfWrithe tangle
			in (if w >= 0 then -b else -a) ^ (3 * abs w)

		skein [] assocs mul =
			let (sch, poly) = reductionOutcome (array (1, numberOfCrossings tangle) assocs)
			in [(sch, mul * poly)]

		skein (c : rest) assocs mul = merge
			(skein rest ((crossingIndex c, False) : assocs) (a * mul))
			(skein rest ((crossingIndex c, True) : assocs) (b * mul))
			where
				merge [] bl = bl
				merge al [] = al
				merge al@(ae@(as, ap) : at) bl@(be@(bs, bp) : bt) =
					case compare as bs of
						LT             -> ae : merge at bl
						GT             -> be : merge al bt
						EQ | s == 0    -> merge at bt
						   | otherwise -> (as, s) : merge at bt
					where
						s = ap + bp

		a = LP.LP [(LP.LM $ Map.fromList [("t", -1 / 4)], 1)]
		b = LP.LP [(LP.LM $ Map.fromList [("t",  1 / 4)], 1)]
		d = -((a * a) + (b * b))

		reductionOutcome :: UArray Int Bool -> (Scheme, Poly)
		reductionOutcome reduction = (scheme, d ^ circles)
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
						smooting drt
							| passOver drt == (reduction ! crossingIndex (incidentCrossing drt))  = nextCCW drt
							| otherwise                                                           = nextCW drt
