{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , minimalKauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanFStateSum
import Math.Topology.KnotTh.Link


class (Knotted k) => KnottedWithKauffmanFPolynomial k where
    type KauffmanFPolynomial k :: *
    kauffmanFPolynomial        :: k DiagramCrossingType -> KauffmanFPolynomial k
    minimalKauffmanFPolynomial :: k DiagramCrossingType -> KauffmanFPolynomial k


instance KnottedWithKauffmanFPolynomial Tangle where
    type KauffmanFPolynomial Tangle = ChordDiagramsSum Poly2
    kauffmanFPolynomial tangle = finalNormalization tangle (reduceSkeinStd tangle)
    minimalKauffmanFPolynomial = skeinRelationPreMinimization kauffmanFPolynomial


instance KnottedWithKauffmanFPolynomial Link where
    type KauffmanFPolynomial Link = Poly2
    kauffmanFPolynomial = takeAsScalar . kauffmanFPolynomial . linkToTangle
    minimalKauffmanFPolynomial = takeAsScalar . minimalKauffmanFPolynomial . linkToTangle


normalizedKauffmanFPolynomialOfLink :: LinkDiagram -> Poly2
normalizedKauffmanFPolynomialOfLink link
    | isEmptyKnotted link  = error "normalizedKauffmanFPolynomialOfLink: empty link provided"
    | otherwise            =
        normalizeBy2
            (twistPFactor * smoothFactor * circleFactor)
            (twistPFactor * smoothFactor * kauffmanFPolynomial link)
