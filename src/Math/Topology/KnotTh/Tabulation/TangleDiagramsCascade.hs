module Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
    ( module Math.Combinatorics.Generation.CanonicalConstructionPath
    , module Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.IncrementalTests
    , module Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.RootingTest
    , nextNumberOfLegs
    , diagonalIndex
    , uniqueGlueSites
    , uniqueGlueSites'
    , primeProjections
    , reducedProjections
    , templateProjections
    , primeDiagrams
    , primeIrreducibleDiagrams
    , primeIrreducibleDiagramsTriangle
    ) where

import Control.Arrow (first)
import Control.Monad (guard)
import Math.Topology.KnotTh.Dihedral.D4 hiding (fromRotation)
import Math.Topology.KnotTh.Dihedral.Dn
import Math.Combinatorics.Generation.CanonicalConstructionPath
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.IncrementalTests
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.RootingTest


{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2


cutInTriangle :: Int -> (Int, Dart Tangle a, x) -> Bool
cutInTriangle maxN (gl, leg, _) =
    let tangle = dartOwner leg
        v = numberOfVertices tangle
        l = numberOfLegs tangle
    in diagonalIndex (1 + v) (nextNumberOfLegs l gl) <= diagonalIndex maxN 4


uniqueGlueSites :: Int -> (Tangle a, (SubGroup Dn, (D4, D4))) -> [(Dart Tangle a, Maybe D4)]
uniqueGlueSites gl (tangle, (symmetry, (adjRot, adjMir))) = do
    let period = rotationPeriod symmetry

        adjointDifferenceForBasis a b
            | needMirror                    = adjRotation ∘ adjMir
            | otherwise                     = adjRotation
            where
                needMirror = reflection a /= reflection b

                toRotate | needMirror  = reflectionBasis symmetry ∘ b
                         | otherwise   = b

                rotationDiff = rotation a - rotation toRotate

                rotationNum =
                    let d = rotationDiff `div` period
                    in if reflection a then -d else d

                adjRotation = power rotationNum adjRot

    (!legIndex, !inducedSymmetry) <-
        case mirroredZero symmetry of
            Nothing  -> [(x, Nothing) | x <- [0 .. period - 1]]
            Just mz0 ->
                let mz = (mz0 + gl - 1) `mod` period

                    getEndpoint doubleIndex =
                        let legIndex = doubleIndex `quot` 2
                            fixup = adjointDifferenceForBasis
                                            (fromRotationReflection (numberOfLegs tangle) (legIndex - gl + 1, True))
                                            (fromRotation (numberOfLegs tangle) legIndex)
                            induced | even doubleIndex  = Just $! fixup ∘ (case gl of { 3 -> d4EC2 ; 2 -> d4EC3 ; _ -> d4E })
                                    | otherwise         = Nothing
                        in (legIndex, induced)

                    leftB = getEndpoint (mz - period)
                    rightB = getEndpoint mz

                    fill !c | c == fst rightB  = [rightB] -- sic!
                            | c == fst leftB   = leftB : fill (c + 1)
                            | otherwise        = (c, Nothing) : fill (c + 1)

                in fill $ fst leftB

    return (nthLeg tangle legIndex, inducedSymmetry)


uniqueGlueSites' :: Int -> (Tangle a, SubGroup Dn) -> [(Dart Tangle a, Maybe D4)]
uniqueGlueSites' gl (tangle, symmetry) = uniqueGlueSites gl (tangle, (symmetry, (d4I, d4I)))


primeProjections, reducedProjections, templateProjections
    :: Int -> CanonicalConstructionPathI
        (TangleProjection, (SubGroup Dn, (D4, D4)))
        (Int, TangleProjectionDart, ProjectionCrossing)
        (TangleProjectionVertex, (SubGroup Dn, (D4, D4)))

primeProjections maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, _) <- uniqueGlueSites gl ts
            return (gl, leg, projectionCrossing)
        , tryAscent = \ (gl, leg, st) -> do
            let root = glueToBorder leg gl st
            (sym, adj, _) <- rootingSymmetryTest root
            return (root, (sym, adj))
        , lowerProjection  = first vertexOwner
        , roots = [(extractTangle4 lonerProjection, (fromPeriodAndMirroredZero 4 1 0, (d4I, d4I)))]
        }

reducedProjections maxN =
    filterUpper (\ _ (gl, leg, _) -> testNoMultiEdges leg gl) $
        primeProjections maxN

templateProjections maxN =
    filterUpper (\ (t, _) (gl, leg, _) -> (numberOfVertices t == 1 || numberOfLegs t > 4) && testNoMultiEdges leg gl) $
        filterLower (\ (gl, _, _) (root, _) -> gl < 3 || testFlow4 root) $
            primeProjections maxN


primeDiagrams, primeIrreducibleDiagrams, primeIrreducibleDiagramsTriangle
    :: Int -> CanonicalConstructionPathI
        (TangleDiagram, (SubGroup Dn, (D4, D4)))
        (Int, TangleDiagramDart, DiagramCrossing)
        (TangleDiagramVertex, (SubGroup Dn, (D4, D4)))

primeDiagrams maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, inducedSymmetry) <- uniqueGlueSites gl ts
            st <- possibleDiagramOrientations inducedSymmetry
            return (gl, leg, st)
        , tryAscent = \ (gl, leg, st) -> do
            let root = glueToBorder leg gl st
            (sym, adj, _) <- rootingSymmetryTest root
            return (root, (sym, adj))
        , lowerProjection  = first vertexOwner
        , roots = [(extractTangle4 lonerOverCrossing, (fromPeriodAndMirroredZero 4 1 0, (d4EC, d4E)))]
        }

primeIrreducibleDiagrams maxN =
    filterUpper (\ _ (gl, leg, st) -> testNo2ndReidemeisterReduction st leg gl)
        $ primeDiagrams maxN

primeIrreducibleDiagramsTriangle maxN =
    filterUpper (const $ cutInTriangle maxN) $ primeIrreducibleDiagrams maxN
