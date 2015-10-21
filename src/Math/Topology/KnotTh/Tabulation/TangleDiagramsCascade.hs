module Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
    ( module Math.Topology.KnotTh.Tabulation.CanonicalConstructionPath
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
import Math.Topology.KnotTh.Algebra.Dihedral.D4 hiding (fromRotation)
import Math.Topology.KnotTh.Algebra.Dihedral.Dn
import Math.Topology.KnotTh.Tabulation.CanonicalConstructionPath
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.IncrementalTests
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.RootingTest
import Math.Topology.KnotTh.Tangle


{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2


cutInTriangle :: Int -> (Tangle x, Int, Int, x) -> Bool
cutInTriangle maxN (tangle, gl, _, _) =
    let v = numberOfVertices tangle
        l = numberOfLegs tangle
    in diagonalIndex (1 + v) (nextNumberOfLegs l gl) <= diagonalIndex maxN 4


uniqueGlueSites :: Int -> (Tangle a, (SubGroup Dn, (D4, D4))) -> [(Int, Maybe D4)]
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

    return (legIndex, inducedSymmetry)


uniqueGlueSites' :: Int -> (Tangle a, SubGroup Dn) -> [(Int, Maybe D4)]
uniqueGlueSites' gl (tangle, symmetry) = uniqueGlueSites gl (tangle, (symmetry, (d4I, d4I)))


primeProjections, reducedProjections, templateProjections
    :: Int -> CanonicalConstructionPathI
                (TangleProjection, (SubGroup Dn, (D4, D4)))
                (TangleProjection, Int, Int)
                (TangleProjectionVertex, (SubGroup Dn, (D4, D4)))

primeProjections maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, _) <- uniqueGlueSites gl ts
            return (tangle, gl, leg)
        , tryAscent = \ (tangle, gl, leg) -> do
            let root = glueToBorder gl (tangle, leg) ProjectionCrossing
            (sym, adj, _) <- rootingSymmetryTest root
            return (root, (sym, adj))
        , lowerProjection  = first vertexOwner
        , roots = [(toTangle lonerProjection, (fromPeriodAndMirroredZero 4 1 0, (d4I, d4I)))]
        }

reducedProjections maxN =
    filterUpper (\ _ (tangle, gl, leg) -> testNoMultiEdges (nthLeg tangle leg) gl) $
        primeProjections maxN

templateProjections maxN =
    filterUpper (\ (t, _) (_, gl, leg) -> (numberOfVertices t == 1 || numberOfLegs t > 4) && testNoMultiEdges (nthLeg t leg) gl) $
        filterLower (\ (_, gl, _) (root, _) -> gl < 3 || testFlow4 root) $
            primeProjections maxN


primeDiagrams, primeIrreducibleDiagrams, primeIrreducibleDiagramsTriangle
    :: Int -> CanonicalConstructionPathI
                (TangleDiagram, (SubGroup Dn, (D4, D4)))
                (TangleDiagram, Int, Int, DiagramCrossing)
                (TangleDiagramVertex, (SubGroup Dn, (D4, D4)))

primeDiagrams maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, inducedSymmetry) <- uniqueGlueSites gl ts
            st <- possibleDiagramOrientations inducedSymmetry
            return (tangle, gl, leg, st)
        , tryAscent = \ (tangle, gl, leg, st) -> do
            let root = glueToBorder gl (tangle, leg) st
            (sym, adj, _) <- rootingSymmetryTest root
            return (root, (sym, adj))
        , lowerProjection  = first vertexOwner
        , roots = [(toTangle lonerOverCrossing, (fromPeriodAndMirroredZero 4 1 0, (d4EC, d4E)))]
        }

primeIrreducibleDiagrams maxN =
    filterUpper (\ _ (tangle, gl, leg, st) -> testNo2ndReidemeisterReduction st (nthLeg tangle leg) gl) $
        primeDiagrams maxN

primeIrreducibleDiagramsTriangle maxN =
    filterUpper (const $ cutInTriangle maxN) $
        primeIrreducibleDiagrams maxN
