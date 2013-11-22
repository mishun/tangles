{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.JonesPolynomial
    ( jonesPolynomial
    , minimalJonesPolynomial
    , normalizedJonesPolynomialOfLink
--    , kauffmanXPolynomial
--    , minimalKauffmanXPolynomialOfLink
    ) where

import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.JonesStateSum
import Math.Topology.KnotTh.Link


class (Knotted k) => KnottedWithJonesPolynomial k where
    type JonesPolynomial k :: *
    jonesPolynomial        :: k ArbitraryCrossing -> JonesPolynomial k
    minimalJonesPolynomial :: k ArbitraryCrossing -> JonesPolynomial k


instance KnottedWithJonesPolynomial Tangle where
    type JonesPolynomial Tangle = JonesStateSum Poly

    jonesPolynomial = reduceSkeinStd

    minimalJonesPolynomial tangle = minimum $ do
        let p = jonesPolynomial tangle
            l = numberOfLegs tangle
        rotation <- if l == 0 then [id] else map rotateState [0 .. l - 1]
        reflection <- [id, mirrorState]
        inv <- [id, invertCrossingsAction]
        return $ inv $ reflection $ rotation p


instance KnottedWithJonesPolynomial Link where
    type JonesPolynomial Link = Poly

    jonesPolynomial link =
        scalarJonesStateSum $
            jonesPolynomial (linkToTangle link)

    minimalJonesPolynomial link =
        scalarJonesStateSum $
            minimalJonesPolynomial (linkToTangle link)


normalizedJonesPolynomialOfLink :: NALink -> Poly
normalizedJonesPolynomialOfLink link
    | isEmptyKnotted link  = error "jonesPolynomialOfLink: empty link provided"
    | otherwise            = normalizeBy (1 + monomial 1 "t" 1) (monomial (-1) "t" (1 / 2) * jonesPolynomial link)


{-
kauffmanXVar :: String
kauffmanXVar = "A"


kauffmanXRelation :: BracketLikeRelation Poly
kauffmanXRelation = BracketLikeRelation (monomial 1 kauffmanXVar 1) (monomial 1 kauffmanXVar (-1))


kauffmanXPolynomial :: (SkeinStructure k) => k ArbitraryCrossing -> ResultOnStructure k PlanarDiagramsSum Poly
kauffmanXPolynomial = evaluateSkeinRelation kauffmanXRelation


minimalKauffmanXPolynomialOfLink :: NALink -> Poly
minimalKauffmanXPolynomialOfLink link =
    let p = kauffmanXPolynomial link
    in min p (invert kauffmanXVar p)
-}