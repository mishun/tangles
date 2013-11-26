{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.JonesPolynomial
    ( jonesPolynomial
    , minimalJonesPolynomial
    , normalizedJonesPolynomialOfLink
    ) where

import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.Invariants.KauffmanXPolynomial


class (Knotted k) => KnottedWithJonesPolynomial k where
    type JonesPolynomial k :: * 
    jonesPolynomial        :: k DiagramCrossingType -> JonesPolynomial k
    minimalJonesPolynomial :: k DiagramCrossingType -> JonesPolynomial k


instance KnottedWithJonesPolynomial Tangle where
    type JonesPolynomial Tangle = KauffmanXStateSum Poly
    jonesPolynomial = fmap kauffmanXToJones . kauffmanXPolynomial
    minimalJonesPolynomial = fmap kauffmanXToJones . minimalKauffmanXPolynomial


instance KnottedWithJonesPolynomial Link where
    type JonesPolynomial Link = Poly
    jonesPolynomial = kauffmanXToJones . kauffmanXPolynomial
    minimalJonesPolynomial = kauffmanXToJones . minimalKauffmanXPolynomial


normalizedJonesPolynomialOfLink :: LinkDiagram -> Poly
normalizedJonesPolynomialOfLink link
    | isEmptyKnotted link  = error "jonesPolynomialOfLink: empty link provided"
    | otherwise            = normalizeBy (1 + monomial 1 "t" 1) (monomial (-1) "t" (1 / 2) * jonesPolynomial link)
