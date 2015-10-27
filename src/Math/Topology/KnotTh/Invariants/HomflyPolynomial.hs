{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.HomflyPolynomial
    ( homflyPolynomial
    , minimalHomflyPolynomial
    ) where

import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanFStateSum
import Math.Topology.KnotTh.Tangle


class (Knotted k) => KnottedWithHomflyPolynomial k where
    type HomflyPolynomial k :: *
    homflyPolynomial        :: k DiagramCrossing -> HomflyPolynomial k
    minimalHomflyPolynomial :: k DiagramCrossing -> HomflyPolynomial k


instance KnottedWithHomflyPolynomial OrientedTangle where
    type HomflyPolynomial OrientedTangle = ChordDiagramsSum Poly2
    homflyPolynomial = error "not implemented"
    minimalHomflyPolynomial = error "not implemented"
