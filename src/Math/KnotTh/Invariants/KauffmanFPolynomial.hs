module Math.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Data.Ratio ((%), numerator)
import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LP
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Link.NonAlternating as L
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


kauffmanFPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly
kauffmanFPolynomial = evaluateSkeinRelation $
    KauffmanFRelation
        (monomial 1 "a" 1) (monomial 1 "a" (-1))
        (monomial 1 "z" 1) (monomial 1 "z" (-1))


normalizedKauffmanFPolynomialOfLink :: L.NonAlternatingLink -> Poly
normalizedKauffmanFPolynomialOfLink link
    | empty      = error "kauffmanFPolynomialOfLink: emptry link provided"
    | otherwise  = let (LP.LP q') = recip big * q in LP.LP $ map (\ (a', b') -> (a', numerator b')) q'
    where
        empty = (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)
        (LP.LP mono) = kauffmanFPolynomial link
        a = LP.var "a"
        z = LP.var "z"
        big = (a * z) ^ (100 :: Int)
        p = LP.LP $ map (\ (a', b') -> (a', b' % 1)) mono
        (q, _) = LP.quotRemLP (big * a * z * p) (a * a + 1 - a * z)
