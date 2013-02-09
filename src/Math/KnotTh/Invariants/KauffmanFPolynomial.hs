module Math.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    , minimalKauffmanFPolynomialOfLink
    , minimalKauffmanFPolynomialOfTangle
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein.Applied
import Math.KnotTh.Invariants.Util.Poly
import Math.KnotTh.Invariants.Util.BruteForceMinimization


a, a', z, z' :: Poly2
a  = monomial 1 "a" 1
a' = monomial 1 "a" (-1)
z  = monomial 1 "z" 1
z' = monomial 1 "z" (-1)


data KauffmanFRelation = KauffmanFRelation


instance SkeinRelation KauffmanFRelation Poly2 where
    circleFactor _ = (a + a') * z' - 1

    initialLplus _ = [(Lplus, 1)]

    twistPFactor _ = a
    twistNFactor _ = a'

    smoothLplusFactor _ = -1
    smoothLzeroFactor _ = z
    smoothLinftyFactor _ = z

    finalNormalization _ knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then a else a') ^ (abs w)
        in (factor *)


kauffmanFPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly2
kauffmanFPolynomial = evaluateSkeinRelation KauffmanFRelation


normalizedKauffmanFPolynomialOfLink :: L.NonAlternatingLink -> Poly2
normalizedKauffmanFPolynomialOfLink link
    | (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)  = error "kauffmanFPolynomialOfLink: emptry link provided"
    | otherwise                                                       = normalizeBy2 (a * a + 1 - a * z) (a * z * kauffmanFPolynomial link)


minimalKauffmanFPolynomialOfLink :: L.NonAlternatingLink -> Poly2
minimalKauffmanFPolynomialOfLink link =
    let p = kauffmanFPolynomial link
    in min p (invert2 "a" p)


minimalKauffmanFPolynomialOfTangle :: T.NonAlternatingTangle -> StateSum Poly2
minimalKauffmanFPolynomialOfTangle tangle = bruteForceMinimumOfTangle kauffmanFPolynomial tangle
{-    | l == 0     = min p (map (fmap $ invert2 "a") p)
    | otherwise  = minimum $ do
        rot <- [0 .. l - 1]
        let rotated = rotateStateSum KauffmanFRelation rot p
            mirrored = mirrorStateSum KauffmanFRelation rotated
        [rotated, fmap (fmap $ invert2 "a") rotated, mirrored, fmap (fmap $ invert2 "a") mirrored]
    where
        p = kauffmanFPolynomial tangle
        l = T.numberOfLegs tangle
-}