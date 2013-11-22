{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , minimalKauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Invariants.Skein
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.Util.BruteForceMinimization


a, a', z, z' :: Poly2
a  = monomial 1 "a" 1
a' = monomial 1 "a" (-1)
z  = monomial 1 "z" 1
z' = monomial 1 "z" (-1)


invertF :: Poly2 -> Poly2
invertF = invert2 "a"


writheFactor :: (SkeinStructure k) => k ArbitraryCrossing -> Poly2
writheFactor knot =
    let w = selfWrithe knot
    in (if w <= 0 then a else a') ^ abs w


data KauffmanFRelation = KauffmanFRelation


instance SkeinRelation KauffmanFRelation Poly2 where
    type SkeinRelationModel KauffmanFRelation = ChordDiagramsSum

    circleFactor _ = (a + a') * z' - 1

    initialLplus _ = [(Lplus, 1)]

    twistPFactor _ = a
    twistNFactor _ = a'

    smoothLplusFactor _ = -1
    smoothLzeroFactor _ = z
    smoothLinftyFactor _ = z

    finalNormalization _ knot = (writheFactor knot *)


class (Knotted k) => KnottedWithKauffmanFPolynomial k where
    type KauffmanFPolynomial k :: *
    kauffmanFPolynomial        :: k ArbitraryCrossing -> KauffmanFPolynomial k
    minimalKauffmanFPolynomial :: k ArbitraryCrossing -> KauffmanFPolynomial k


instance KnottedWithKauffmanFPolynomial Tangle where
    type KauffmanFPolynomial Tangle = ChordDiagramsSum Poly2

    kauffmanFPolynomial = evaluateSkeinRelation KauffmanFRelation

    minimalKauffmanFPolynomial = bruteForceMinimumOfTangle kauffmanFPolynomial {-tangle
        | l == 0     =
            let p = kauffmanFPolynomial tangle
            in min p $ fmap invertF p
        | otherwise  = minimum $ do
            let wf = writheFactor tangle
                wf' = invertF wf
                p = fmap (* wf') $ kauffmanFPolynomial tangle
            rot <- [0 .. l - 1]
            let rotated = fmap (* wf) $ rotateChordDiagramsSum KauffmanFRelation rot p
                mirrored = mirrorChordDiagramsSum KauffmanFRelation $ fmap invertF rotated
                r = kauffmanFPolynomial $ T.rotateTangle rot $ invertCrossings tangle
                s = kauffmanFPolynomial $ T.mirrorTangle $ T.rotateTangle rot $ invertCrossings tangle
            [rotated, mirrored, r, s]
        where
            l = T.numberOfLegs tangle-}


instance KnottedWithKauffmanFPolynomial Link where
    type KauffmanFPolynomial Link = Poly2

    kauffmanFPolynomial = evaluateSkeinRelation KauffmanFRelation

    minimalKauffmanFPolynomial link =
        let p = kauffmanFPolynomial link
        in min p (invertF p)


normalizedKauffmanFPolynomialOfLink :: NALink -> Poly2
normalizedKauffmanFPolynomialOfLink link
    | isEmptyKnotted link  = error "normalizedKauffmanFPolynomialOfLink: empty link provided"
    | otherwise            = normalizeBy2 (a * a + 1 - a * z) (a * z * kauffmanFPolynomial link)
