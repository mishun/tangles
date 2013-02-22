{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Invariants.JonesPolynomial
    ( jonesPolynomial
    , normalizedJonesPolynomialOfLink
    , minimalJonesPolynomialOfLink
    , minimalJonesPolynomialOfTangle
    , kauffmanXPolynomial
    , minimalKauffmanXPolynomialOfLink
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein
import Math.KnotTh.Invariants.Util.Poly


data BracketLikeRelation a = BracketLikeRelation a a


instance (Ord a, Num a, Show a) => SkeinRelation (BracketLikeRelation a) a where
    type SkeinRelationModel (BracketLikeRelation a) = PlanarDiagramsSum

    circleFactor (BracketLikeRelation a b) = -(a * a + b * b)

    initialLplus (BracketLikeRelation a b) = [(Lzero, a), (Linfty, b)]

    twistPFactor = undefined
    twistNFactor = undefined

    smoothLplusFactor  = undefined
    smoothLzeroFactor  = undefined
    smoothLinftyFactor = undefined

    finalNormalization (BracketLikeRelation a b) knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then -a else -b) ^ abs (3 * w)
        in (factor *)


jonesVar :: String
jonesVar = "t"


jonesRelation :: BracketLikeRelation Poly
jonesRelation = BracketLikeRelation (monomial 1 jonesVar (-1 / 4)) (monomial 1 jonesVar (1 / 4))


jonesPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> ResultOnStructure k PlanarDiagramsSum Poly
jonesPolynomial = evaluateSkeinRelation jonesRelation


normalizedJonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
normalizedJonesPolynomialOfLink link
    | (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)  =
        error "jonesPolynomialOfLink: empty link provided"
    | otherwise                                                       =
        normalizeBy (1 + monomial 1 jonesVar 1) (monomial (-1) jonesVar (1 / 2) * jonesPolynomial link)


minimalJonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalJonesPolynomialOfLink link =
    let p = jonesPolynomial link
    in min p (invert jonesVar p)


minimalJonesPolynomialOfTangle :: T.NonAlternatingTangle -> PlanarDiagramsSum Poly
minimalJonesPolynomialOfTangle tangle
    | l == 0     = min p $ fmap (invert jonesVar) p
    | otherwise  = minimum $ do
        rot <- [0 .. l - 1]
        let rotated = rotate jonesRelation rot p
            mirrored = mirror jonesRelation rotated
        [rotated, fmap (invert jonesVar) rotated, mirrored, fmap (invert jonesVar) mirrored]
    where
        p = jonesPolynomial tangle
        l = T.numberOfLegs tangle


kauffmanXVar :: String
kauffmanXVar = "A"


kauffmanXRelation :: BracketLikeRelation Poly
kauffmanXRelation = BracketLikeRelation (monomial 1 kauffmanXVar 1) (monomial 1 kauffmanXVar (-1))


kauffmanXPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> ResultOnStructure k PlanarDiagramsSum Poly
kauffmanXPolynomial = evaluateSkeinRelation kauffmanXRelation


minimalKauffmanXPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalKauffmanXPolynomialOfLink link =
    let p = kauffmanXPolynomial link
    in min p (invert kauffmanXVar p)
