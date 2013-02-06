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


data KauffmanFRelation a = KauffmanFRelation a a a a


instance (Ord a, Num a, Show a) => SkeinRelation (KauffmanFRelation a) a where
    circleFactor (KauffmanFRelation a a' _ z') = (a + a') * z' - 1

    initialLplus _ = InitialSum { ofLplus = 1, ofLzero = 0, ofLinfty = 0 }

    twistPFactor (KauffmanFRelation a _ _ _) = a
    twistNFactor (KauffmanFRelation _ a' _ _) = a'

    smoothLplusFactor _ = -1
    smoothLzeroFactor (KauffmanFRelation _ _ z _) = z
    smoothLinftyFactor (KauffmanFRelation _ _ z _) = z

    finalNormalization (KauffmanFRelation a a' _ _) knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then a else a') ^ (abs w)
        in (factor *)


kauffmanFPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly2
kauffmanFPolynomial = evaluateSkeinRelation $
    KauffmanFRelation
        (monomial2 1 "a" 1) (monomial2 1 "a" (-1))
        (monomial2 1 "z" 1) (monomial2 1 "z" (-1))


normalizedKauffmanFPolynomialOfLink :: L.NonAlternatingLink -> Poly2
normalizedKauffmanFPolynomialOfLink link
    | (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)  = error "kauffmanFPolynomialOfLink: emptry link provided"
    | otherwise                                                       = normalizeKauffmanF $ kauffmanFPolynomial link


bruteForceKauffmanF :: L.NonAlternatingLink -> Poly2
bruteForceKauffmanF link =
    let relation = KauffmanFRelation (monomial2 1 "a" 1) (monomial2 1 "a" (-1)) (monomial2 1 "z" 1) (monomial2 1 "z" (-1)) 
    in normalizeKauffmanF $ finalNormalization relation link $
        takeAsConst $ decomposeTangle relation 1 $ T.fromLink link
