{-# LANGUAGE Rank2Types #-}
module Math.Topology.KnotTh.Enumeration.Applied.NonAlternatingTangles
    ( module X
    , tangleDiagrams
    , tangleClasses
    , weakTangleClasses
    , siftTangles
    , siftWeakTangles
    , lookingForwardTanglesEnumeration
    , lookingForwardWeakTanglesEnumeration
    ) where

import Control.Monad (when)
import Text.Printf
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant as X
import Math.Topology.KnotTh.Enumeration.DiagramInfo as X
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Tangle as X
import Math.Topology.KnotTh.Tangle.NonAlternating.Satellites
import Math.Topology.KnotTh.Tangle.IsomorphismTest
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
import Math.Topology.KnotTh.Link (tangleDoubling)
import Math.Topology.KnotTh.Invariants
import qualified Math.Topology.KnotTh.Tangle.Moves.Flype as Flype
import qualified Math.Topology.KnotTh.Tangle.Moves.Pass as Pass
import qualified Math.Topology.KnotTh.Tangle.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.Topology.KnotTh.Tangle.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.Topology.KnotTh.Tangle.Moves.Weak as Weak


tangleDiagrams :: (Monad m) => Bool -> Int -> Int -> (NATangle -> m ()) -> m ()
tangleDiagrams triangle legsLimit maxN yield =
    let path | triangle   = primeIrreducibleDiagramsTriangle maxN
             | otherwise  = primeIrreducibleDiagrams maxN
    in forCCP_ path $ \ (tangle, _) ->
        when (legsLimit < 0 || numberOfLegs tangle <= legsLimit) $
            yield tangle


tangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NATangle -> m ()) -> m ()) -> [info NATangle]
tangleClasses =
    equivalenceClasses
        isomorphismTest
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


weakTangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NATangle -> m ()) -> m ()) -> [info NATangle]
weakTangleClasses =
    equivalenceClasses
        isomorphismTest
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ Weak.neighbours
            , ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


siftTangles :: (DiagramInfo info) => [info NATangle] -> SiftResult info NATangle
siftTangles = siftByInvariant $ \ tangle ->
    ( minimalJonesPolynomial tangle
    , minimalJonesPolynomial $ twistedDoubleSatellite tangle
--    , minimalKauffmanFPolynomial $ twistedTripleSatellite tangle
    )


siftWeakTangles :: (DiagramInfo info) => [info NATangle] -> SiftResult info NATangle
siftWeakTangles = siftByInvariant $ \ tangle ->
    ( jonesPolynomial $ tangleDoubling id tangle
    , jonesPolynomial $ tangleDoubling id $ twistedDoubleSatellite tangle
    , kauffmanFPolynomial $ tangleDoubling id tangle
    )


lookingForwardTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo NATangle
lookingForwardTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftTangles $ filter ((<= n) . numberOfVertices . representative) $
            tangleClasses $ tangleDiagrams triangle legsLimit (n + forward)


lookingForwardWeakTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo NATangle
lookingForwardWeakTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardWeakTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftWeakTangles $ filter ((<= n) . numberOfVertices . representative) $
            weakTangleClasses $ tangleDiagrams triangle legsLimit (n + forward)
