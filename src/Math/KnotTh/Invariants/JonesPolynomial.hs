module Math.KnotTh.Invariants.JonesPolynomial
	( jonesPolynomial
	, jonesPolynomialOfLink
	, kauffmanXPolynomial
	, minimalJonesPolynomialOfLink
	, minimalKauffmanXPolynomialOfLink
	, jonesPolynomialOfTangle
	, minimalJonesPolynomialOfTangle
	) where

import Data.Ratio ((%), numerator)
import Data.List (sort, foldl')
import Data.Array.Unboxed (UArray, array, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LP
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein.Applied


type Poly = LP.LaurentMPoly Int


monomial :: Int -> String -> B.Q -> Poly
monomial a var d = LP.LP [(LP.LM $ M.fromList [(var, d)], a)]


invert :: String -> Poly -> Poly
invert var (LP.LP monomials) = sum $ do
	(LP.LM vars, coeff) <- monomials
	let modify p@(x, d)
		| x == var   = (x, -d)
		| otherwise  = p
	return $! LP.LP [(LP.LM $ M.fromList $ map modify $ M.toList vars, coeff)]


data BracketLikeRelation a = BracketLikeRelation a a


instance (Ord a, Num a, Show a) => SkeinRelation (BracketLikeRelation a) a where
	circleFactor (BracketLikeRelation a b) = -(a * a + b * b)

	initialLplus (BracketLikeRelation a b) = InitialSum { ofLplus = 0, ofLzero = a, ofLinfty = b }

	twistPFactor = undefined

	twistNFactor = undefined

	finalNormalization (BracketLikeRelation a b) knot =
		let factor =
			let w = selfWrithe knot
			in (if w <= 0 then -a else -b) ^ abs (3 * w)
		in (factor *)


jonesVar, kauffmanXVar :: String
jonesVar = "t"
kauffmanXVar = "A"


jonesPolynomial :: (SkeinResult Poly r k c d) => k ArbitraryCrossing -> r
jonesPolynomial = evaluateSkeinRelation $ BracketLikeRelation (monomial 1 jonesVar (-1 / 4)) (monomial 1 jonesVar (1 / 4))


jonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
jonesPolynomialOfLink link
	| empty      = error "jonesPolynomialOfLink: empty link provided"
	| otherwise  = let (LP.LP q') = recip big * q in LP.LP $ map (\ (a, b) -> (a, numerator b)) q'
	where
		empty = (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)
		(LP.LP mono) = jonesPolynomial link
		t = LP.var jonesVar
		big = t ^ (100 :: Int)
		p = LP.LP $ map (\ (a, b) -> (a, b % 1)) mono
		(q, _) = LP.quotRemLP (big * p * (-LP.sqrtvar jonesVar)) (1 + t)


kauffmanXPolynomial :: (SkeinResult Poly r k c d) => k ArbitraryCrossing -> r
kauffmanXPolynomial = evaluateSkeinRelation $ BracketLikeRelation (monomial 1 kauffmanXVar 1) (monomial 1 kauffmanXVar (-1))


minimalJonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalJonesPolynomialOfLink link =
	let jp = jonesPolynomial link
	in min jp (invert jonesVar jp)


minimalKauffmanXPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalKauffmanXPolynomialOfLink link =
	let kp = kauffmanXPolynomial link
	in min kp (invert kauffmanXVar kp)


type Scheme = [(Int, Int)]


jonesPolynomialOfTangle :: T.NonAlternatingTangle -> [(Scheme, Poly)]
jonesPolynomialOfTangle tangle = map (\ (sch, poly) -> (sch, wm * cm * poly)) $ skein (allCrossings tangle) [] 1
	where
		jonesA = monomial 1 jonesVar (-1 / 4)
		jonesB = monomial 1 jonesVar (1 / 4)
		jonesD = -(jonesA * jonesA + jonesB * jonesB)

		cm = jonesD ^ numberOfFreeLoops tangle

		wm =	let w = selfWrithe tangle
			in (if w >= 0 then -jonesB else -jonesA) ^ (3 * abs w)

		skein [] assocs mul =
			let (sch, poly) = reductionOutcome (array (crossingIndexRange tangle) assocs)
			in [(sch, mul * poly)]

		skein (c : rest) assocs mul = merge
			(skein rest ((crossingIndex c, False) : assocs) (jonesA * mul))
			(skein rest ((crossingIndex c, True) : assocs) (jonesB * mul))
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

		reductionOutcome :: UArray Int Bool -> (Scheme, Poly)
		reductionOutcome reduction = (scheme, jonesD ^ circles)
			where
				circles = (length paths) - (length pairs)

				scheme =
					let toPositionPair (al, bl) = (min ap bp, max ap bp)
						where
							ap = T.legPlace al
							bp = T.legPlace bl
					in sort $ map toPositionPair pairs

				pairs = map (\ path -> (fst $ head path, snd $ last path)) $ filter (T.isLeg . fst . head) paths

				paths =
					let smooting drt
						| passOver drt == (reduction ! crossingIndex (incidentCrossing drt))  = nextCCW drt
						| otherwise                                                           = nextCW drt
					in undirectedPathsDecomposition smooting tangle


		containingDirectedPath (adjForward, adjBackward) start
			| isCycle    = forward
			| otherwise  = walkBackward (start, forward)
			where
				(forward, isCycle) = walkForward start

				walkForward d
					| T.isLeg opp   = ([d], False)
					| start == nxt  = ([d], True)
					| otherwise     = (d : nextPath, nextCycle)
					where
						opp = opposite d
						nxt = adjForward opp
						(nextPath, nextCycle) = walkForward nxt

				walkBackward (d, path)
					| T.isLeg d  = path
					| otherwise  = let prev = opposite $ adjBackward d in walkBackward (prev, prev : path)

		undirectedPathsDecomposition continue = fst . foldl' processDart ([], S.empty) . allHalfEdges
			where
				processDart (!paths, s) d
					| S.member d s  = (paths, s)
					| otherwise     = (path : paths, nextS)
					where
						path = containingUndirectedPath continue d
						nextS = foldl' (\ curs (a, b) -> S.insert b $ S.insert a curs) s path

				containingUndirectedPath cont = map (\ d -> (d, opposite d)) . containingDirectedPath (cont, cont)


minimalJonesPolynomialOfTangle :: T.NonAlternatingTangle -> [(Scheme, Poly)]
minimalJonesPolynomialOfTangle tangle = minimum $ do
	let jp = jonesPolynomialOfTangle tangle
	let l = T.numberOfLegs tangle
	rot <- [0 .. l - 1]

	f <-
		let mapScheme f = map $ \ (a, b) -> 
			let a' = f a `mod` l
			    b' = f b `mod` l
			in (min a' b', max a' b')
		in
			[ \ (s, p) -> (sort $ mapScheme (+ rot) s, p)
			, \ (s, p) -> (sort $ mapScheme (+ rot) s, invert jonesVar p)
			, \ (s, p) -> (sort $ mapScheme (\ i -> rot - i) s, p)
			, \ (s, p) -> (sort $ mapScheme (\ i -> rot - i) s, invert jonesVar p)
			]

	return $! sort $! map f jp
