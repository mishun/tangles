{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanFPolynomial
    ( kauffmanFPolynomial
    , minimalKauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanFStateSum
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import Math.Topology.KnotTh.Tangle


class (Knotted k) => KnottedWithKauffmanFPolynomial k where
    type KauffmanFPolynomial k :: *
    kauffmanFPolynomial        :: k DiagramCrossing -> KauffmanFPolynomial k
    minimalKauffmanFPolynomial :: k DiagramCrossing -> KauffmanFPolynomial k


instance KnottedWithKauffmanFPolynomial Tangle where
    type KauffmanFPolynomial Tangle = ChordDiagramsSum Poly2

    kauffmanFPolynomial tangle =
        let factor =
                let writheFactor = twistFactor (-totalSelfWrithe' tangle)
                    loopsFactor = loopFactor ^ numberOfFreeLoops tangle
                in writheFactor * loopsFactor
        in (factor *) `fmap` reduceSkein tangle

    minimalKauffmanFPolynomial = skeinRelationPreMinimization kauffmanFPolynomial


instance KnottedWithKauffmanFPolynomial Tangle0 where
    type KauffmanFPolynomial Tangle0 = Poly2
    kauffmanFPolynomial = takeAsScalar . kauffmanFPolynomial . toTangle
    minimalKauffmanFPolynomial = takeAsScalar . minimalKauffmanFPolynomial . toTangle


normalizedKauffmanFPolynomialOfLink :: LinkDiagram -> Poly2
normalizedKauffmanFPolynomialOfLink link | isEmpty    = error "normalizedKauffmanFPolynomialOfLink: empty link provided"
                                         | otherwise  = normalizeBy2 (common * loopFactor) (common * kauffmanFPolynomial link)
    where isEmpty = numberOfVertices link == 0 && numberOfFreeLoops link == 0
          common = twistFactor 1 * smoothFactor
