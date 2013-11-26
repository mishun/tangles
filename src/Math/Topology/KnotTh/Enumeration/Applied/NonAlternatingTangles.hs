{-# LANGUAGE RankNTypes #-}
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
import Math.Topology.KnotTh.Tangle.Satellites
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
import Math.Topology.KnotTh.Link (tangleDoubling)
import Math.Topology.KnotTh.Invariants
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.Flype as Flype
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.Pass as Pass
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.ReidemeisterIII as ReidemeisterIII
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.Weak as Weak


tangleDiagrams :: (Monad m) => Bool -> Int -> Int -> (TangleDiagram -> m ()) -> m ()
tangleDiagrams triangle legsLimit maxN yield =
    let path | triangle   = primeIrreducibleDiagramsTriangle maxN
             | otherwise  = primeIrreducibleDiagrams maxN
    in forCCP_ path $ \ (tangle, _) ->
        when (legsLimit < 0 || numberOfLegs tangle <= legsLimit) $
            yield tangle


tangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (TangleDiagram -> m ()) -> m ()) -> [info TangleDiagram]
tangleClasses =
    equivalenceClasses
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


weakTangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (TangleDiagram -> m ()) -> m ()) -> [info TangleDiagram]
weakTangleClasses =
    equivalenceClasses
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ Weak.neighbours
            , ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


siftTangles :: (DiagramInfo info) => [info TangleDiagram] -> SiftResult info TangleDiagram
siftTangles = siftByInvariant $ \ tangle ->
    ( minimalJonesPolynomial tangle
    , minimalJonesPolynomial $ twistedDoubleSatellite tangle
--    , minimalKauffmanFPolynomial $ twistedTripleSatellite tangle
    )


siftWeakTangles :: (DiagramInfo info) => [info TangleDiagram] -> SiftResult info TangleDiagram
siftWeakTangles = siftByInvariant $ \ tangle ->
    ( jonesPolynomial $ tangleDoubling id tangle
    , jonesPolynomial $ tangleDoubling id $ twistedDoubleSatellite tangle
    , kauffmanFPolynomial $ tangleDoubling id tangle
    )


lookingForwardTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo TangleDiagram
lookingForwardTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftTangles $ filter ((<= n) . numberOfVertices . representative) $
            tangleClasses $ tangleDiagrams triangle legsLimit (n + forward)


lookingForwardWeakTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo TangleDiagram
lookingForwardWeakTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardWeakTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftWeakTangles $ filter ((<= n) . numberOfVertices . representative) $
            weakTangleClasses $ tangleDiagrams triangle legsLimit (n + forward)
