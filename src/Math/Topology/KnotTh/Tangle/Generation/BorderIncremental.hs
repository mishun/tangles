{-# LANGUAGE UnboxedTuples, TypeFamilies #-}
module Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
    ( module X
    , nextNumberOfLegs
    , diagonalIndex
    , uniqueGlueSites
    , uniqueGlueSites'
    , primeProjections
    , reducedProjections
    , templateProjections
    , primeIrreducibleDiagrams
    , primeIrreducibleDiagramsTriangle
    ) where

import Control.Arrow (first)
import Control.Monad (guard)
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.Combinatorics.Generation.CanonicalConstructionPath as X
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental.IncrementalTests as X
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental.RootingTest as X


{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2


uniqueGlueSites :: Int -> (Tangle ct, (Dn.DnSubGroup, (D4.D4, D4.D4))) -> [(Dart Tangle ct, Maybe D4.D4)]
uniqueGlueSites gl (tangle, (symmetry, (adjRot, adjMir))) = do
    let period = Dn.rotationPeriod symmetry

        adjointDifferenceForBasis a b
            | needMirror                    = adjRotation D4.<*> adjMir
            | otherwise                     = adjRotation
            where
                needMirror = Dn.reflection a /= Dn.reflection b

                toRotate | needMirror  = Dn.reflectionBasis symmetry Dn.<*> b
                         | otherwise   = b

                rotationDiff = Dn.rotation a - Dn.rotation toRotate

                rotationNum =
                    let d = rotationDiff `div` period
                    in if Dn.reflection a then -d else d

                adjRotation = D4.power rotationNum adjRot

    (!legIndex, !inducedSymmetry) <-
        if not $ Dn.hasReflectionPart symmetry
            then [(x, Nothing) | x <- [0 .. period - 1]]
            else
                let mz = (Dn.mirroredZero symmetry + gl - 1) `mod` period

                    getEndpoint doubleIndex =
                        let legIndex = doubleIndex `quot` 2
                            fixup = adjointDifferenceForBasis
                                            (Dn.fromRotationReflection (numberOfLegs tangle) (legIndex - gl + 1, True))
                                            (Dn.fromRotation (numberOfLegs tangle) legIndex)
                            induced | even doubleIndex  = Just $! fixup D4.<*> (case gl of { 3 -> D4.ec2 ; 2 -> D4.ec3 ; _ -> D4.e })
                                    | otherwise         = Nothing
                        in (legIndex, induced)

                    leftB = getEndpoint (mz - period)
                    rightB = getEndpoint mz

                    fill !c | c == fst rightB  = [rightB] -- sic!
                            | c == fst leftB   = leftB : fill (c + 1)
                            | otherwise        = (c, Nothing) : fill (c + 1)

                in fill $ fst leftB 

    return (nthLeg tangle legIndex, inducedSymmetry)


uniqueGlueSites' :: Int -> (Tangle ct, Dn.DnSubGroup) -> [(Dart Tangle ct, Maybe D4.D4)]
uniqueGlueSites' gl (tangle, symmetry) = uniqueGlueSites gl (tangle, (symmetry, (D4.i, D4.i)))


primeProjections, reducedProjections, templateProjections
    :: Int -> CanonicalConstructionPathI
        (TangleProj, Dn.DnSubGroup)
        (Int, TangleProjDart, ProjectionCrossing)
        (TangleProjVertex, Dn.DnSubGroup)

primeProjections maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ (tangle, symmetry) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, inducedSymmetry) <- uniqueGlueSites' gl (tangle, symmetry)
            state <- possibleOrientations undefined inducedSymmetry
            return (gl, leg, state)

        , tryAscent        = \ (gl, leg, st) -> do
            let root = glueToBorder leg gl st
            (sym, _, _) <- rootingSymmetryTest root
            return (root, sym)

        , lowerProjection  = first vertexOwner
        , roots            = [(lonerProjection, Dn.fromPeriodAndMirroredZero 4 1 0)]
        }

reducedProjections maxN =
    filterUpper (\ _ (gl, leg, _) -> testNoMultiEdges leg gl) $
        primeProjections maxN


templateProjections maxN =
    filterUpper (\ (t, _) (gl, leg, _) -> (numberOfVertices t == 1 || numberOfLegs t > 4) && testNoMultiEdges leg gl) $
        filterLower (\ (gl, _, _) (root, _) -> gl < 3 || testFlow4 root) $
            primeProjections maxN


primeIrreducibleDiagrams, primeIrreducibleDiagramsTriangle
    :: Int -> CanonicalConstructionPathI
        (TangleDiagram, (Dn.DnSubGroup, (D4.D4, D4.D4)))
        (Int, TangleDiagramDart, DiagramCrossing)
        (TangleDiagram, (Dn.DnSubGroup, (D4.D4, D4.D4)))

primeIrreducibleDiagrams maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            (leg, inducedSymmetry) <- uniqueGlueSites gl ts
            state <- possibleOrientations undefined inducedSymmetry
            return (gl, leg, state)

        , tryAscent        = \ (gl, leg, st) -> do
            guard $ testNo2ndReidemeisterReduction st leg gl
            let root = glueToBorder leg gl st
            (sym, adj, _) <- rootingSymmetryTest root
            return (vertexOwner root, (sym, adj))

        , lowerProjection  = id
        , roots            = [(lonerOverCrossing, (Dn.fromPeriodAndMirroredZero 4 1 0, (D4.ec, D4.e)))]
        }


cutInTriangle :: Int -> (Int, Dart Tangle ct, a) -> Bool
cutInTriangle maxN (gl, leg, _) =
    let tangle = dartOwner leg
        v = numberOfVertices tangle
        l = numberOfLegs tangle
    in diagonalIndex (1 + v) (nextNumberOfLegs l gl) <= diagonalIndex maxN 4


primeIrreducibleDiagramsTriangle maxN =
    filterUpper (const $ cutInTriangle maxN) $ primeIrreducibleDiagrams maxN
