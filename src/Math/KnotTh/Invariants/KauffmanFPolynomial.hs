module Math.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    , bruteForceKauffmanF
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle as T
import Math.KnotTh.Invariants.Skein.Applied
import Math.KnotTh.Invariants.Skein.StateSum.TangleRelation
import Math.KnotTh.Invariants.Util.Poly


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
    | otherwise                                                       = normalizeKauffmanF $ kauffmanFPolynomial link


bruteForceKauffmanF :: L.NonAlternatingLink -> Poly2
bruteForceKauffmanF link =
    normalizeKauffmanF $ finalNormalization KauffmanFRelation link $
        takeAsConst $ decomposeTangle KauffmanFRelation 1 $ T.fromLink link


normalizeKauffmanF :: Poly2 -> Poly2
normalizeKauffmanF poly = normalizeBy2 (a * a + 1 - a * z) (a * z * poly)
