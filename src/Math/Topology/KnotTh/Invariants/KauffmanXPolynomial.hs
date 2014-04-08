{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanXPolynomial
    ( kauffmanXPolynomial
    , minimalKauffmanXPolynomial
    ) where

import Text.Printf
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction


class (Knotted k) => KnottedWithKauffmanXPolynomial k where
    type KauffmanXPolynomial k :: *
    kauffmanXPolynomial        :: k DiagramCrossing -> KauffmanXPolynomial k
    minimalKauffmanXPolynomial :: k DiagramCrossing -> KauffmanXPolynomial k


instance KnottedWithKauffmanXPolynomial Tangle where
    type KauffmanXPolynomial Tangle = KauffmanXStateSum Poly
    kauffmanXPolynomial tangle = finalNormalization tangle (reduceSkeinStd tangle)
    minimalKauffmanXPolynomial = skeinRelationPostMinimization kauffmanXPolynomial


instance KnottedWithKauffmanXPolynomial Link where
    type KauffmanXPolynomial Link = Poly
    kauffmanXPolynomial = takeAsScalar . kauffmanXPolynomial . linkToTangle
    minimalKauffmanXPolynomial = takeAsScalar . minimalKauffmanXPolynomial . linkToTangle


instance KnottedWithKauffmanXPolynomial EmbeddedLink where
    type KauffmanXPolynomial EmbeddedLink = [((Int, Int), Poly2)]

    kauffmanXPolynomial link
        | eulerChar link == 2         = [((0, 0), kauffmanXPolynomial (toLink link))]
        | numberOfVertices link == 0  = [((0, 0), kauffmanXPolynomial (toLink link))]
        | eulerChar link == 0         = torusDecomposition link
        | otherwise  =
            error $ printf "kauffmanXPolynomial: yet implemented for torus only (%i)\n%s" (eulerChar link) (show $ explode link)

    minimalKauffmanXPolynomial link =
        min (kauffmanXPolynomial link) (kauffmanXPolynomial $ invertCrossings link)
