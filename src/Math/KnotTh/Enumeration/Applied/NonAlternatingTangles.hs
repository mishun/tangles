{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
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
import Math.KnotTh.Enumeration.EquivalenceClasses
import Math.KnotTh.Enumeration.SiftByInvariant as X
import Math.KnotTh.Enumeration.DiagramInfo as X
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Tangle.NonAlternating as X
import Math.KnotTh.Tangle.NonAlternating.Satellites
import Math.KnotTh.Tangle.IsomorphismTest
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Link (tangleDoubling)
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.KauffmanFPolynomial
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.Util.ThreadExpansion
import qualified Math.KnotTh.Tangle.Moves.Flype as Flype
import qualified Math.KnotTh.Tangle.Moves.Pass as Pass
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangle.Moves.Weak as Weak


tangleDiagrams :: (Monad m) => Bool -> Int -> Int -> (NonAlternatingTangle -> m ()) -> m ()
tangleDiagrams triangle legsLimit n yield =
    let t | triangle   = triangleBoundedType n primeIrreducibleDiagramType
          | otherwise  = primeIrreducibleDiagramType
    in simpleIncrementalGenerator t [ArbitraryCrossing] n $ \ tangle _ ->
        when (legsLimit < 0 || numberOfLegs tangle <= legsLimit) $
            yield tangle


tangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [info NonAlternatingTangle]
tangleClasses =
    equivalenceClasses
        (\ t -> min (isomorphismTest t) (isomorphismTest $ invertCrossings t))
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


weakTangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [info NonAlternatingTangle]
weakTangleClasses =
    equivalenceClasses
        (\ t -> min (isomorphismTest t) (isomorphismTest $ invertCrossings t))
        (map (map ReidemeisterReduction.greedy1st2ndReduction .)
            [ Weak.neighbours
            , ReidemeisterIII.neighbours
            , Flype.neighbours
            , Pass.neighbours
            ])


siftTangles :: (DiagramInfo info) => [info NonAlternatingTangle] -> SiftResult info NonAlternatingTangle
siftTangles = siftByInvariant $ \ tangle ->
    ( linkingNumbersSet tangle
    , threadExpansion minimalJonesPolynomialOfTangle tangle
    , minimalJonesPolynomialOfTangle $ twistedDoubleSatellite tangle
    , minimalKauffmanFPolynomialOfTangle $ twistedTripleSatellite tangle
    )


siftWeakTangles :: (DiagramInfo info) => [info NonAlternatingTangle] -> SiftResult info NonAlternatingTangle
siftWeakTangles = siftByInvariant $ \ tangle ->
    jonesPolynomial $ tangleDoubling id tangle


lookingForwardTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo NonAlternatingTangle
lookingForwardTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftTangles $ filter ((<= n) . numberOfCrossings . representative) $
            tangleClasses $ tangleDiagrams triangle legsLimit (n + forward)


lookingForwardWeakTanglesEnumeration :: Bool -> Int -> Int -> Int -> SiftResult MinimalDiagramInfo NonAlternatingTangle
lookingForwardWeakTanglesEnumeration triangle legsLimit forward n
    | forward < 0  = error $ printf "lookingForwardWeakTanglesEnumeration: number of forward steps must be non-negative, but %i received" forward
    | otherwise    =
        siftWeakTangles $ filter ((<= n) . numberOfCrossings . representative) $
            weakTangleClasses $ tangleDiagrams triangle legsLimit (n + forward)
