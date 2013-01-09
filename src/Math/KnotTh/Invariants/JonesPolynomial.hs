module Math.KnotTh.Invariants.JonesPolynomial
	( jonesPolynomial
	, kauffmanXPolynomial
	, jonesPolynomialOfLink
	, minimalJonesPolynomialOfLink
	, minimalKauffmanXPolynomialOfLink
	, minimalJonesPolynomialOfTangle
	) where

import Data.Ratio ((%), numerator)
import Data.List (sort)
import Data.Array.Base ((!))
import Data.Array.ST (runSTUArray, newArray_, writeArray)
import qualified Data.Map as M
import Control.Monad (forM_)
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


minimalJonesPolynomialOfTangle :: T.NonAlternatingTangle -> StateSum Poly
minimalJonesPolynomialOfTangle tangle = minimum $ do
	let jp = jonesPolynomial tangle
	let l = T.numberOfLegs tangle
	rot <- [0 .. l - 1]

	let mapSum fx fm s = sort $ flip map s $ \ (StateSummand x m) ->
		flip StateSummand (fm m) $ runSTUArray $ do
			x' <- newArray_ (0, l - 1)
			forM_ [0 .. l - 1] $ \ i ->
				writeArray x' (fx i `mod` l) (fx (x ! i) `mod` l)
			return $! x'

	f <-	[ mapSum (+ rot) id
		, mapSum (+ rot) (invert jonesVar)
		, mapSum (\ i -> rot - i) id
		, mapSum (\ i -> rot - i) (invert jonesVar)
		]

	return $! f jp
