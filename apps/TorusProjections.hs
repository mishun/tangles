{-# LANGUAGE UnboxedTuples #-}
module Main where

import Data.Function (on, fix)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy, nubBy)
import Data.Bits ((.&.), shiftL)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Arrow ((&&&))
import Control.Monad (when, forM_, guard, void)
import Text.Printf
import System.Environment (getArgs)
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.D4 as D4
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, listChordDiagrams, genusOfChordDiagram)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TestPrime
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Draw
import TestUtil.Table


p0 :: (Crossing a) => a -> Dart EmbeddedLink a -> ((Int, UV.Vector Int), EmbeddedLink a)
p0 cross ab =
    let link = dartOwner ab
        ba = opposite ab
        cd = nextCCW ab
        dc = opposite cd
        n = 1 + numberOfVertices link

        res = implode
            ( numberOfFreeLoops link
            , let opp' x | x == ab    = (n, 0)
                         | x == ba    = (n, 1)
                         | x == dc    = (n, 2)
                         | x == cd    = (n, 3)
                         | otherwise  = endPair' x
              in map (\ v -> (map opp' $ outcomingDarts v, vertexCrossing v)) (allVertices link)
                     ++ [(map beginPair' [ab, ba, dc, cd], cross)]
            )

        rc = let v = nthVertex res n
             in min (rootCode' (nthOutcomingDart v 3) R.ccw)
                    (rootCode' (nthOutcomingDart v 0) R.cw)

    in ((2, rc), res)


p1 :: (Crossing a) => a -> Dart EmbeddedLink a -> ((Int, UV.Vector Int), EmbeddedLink a)
p1 cross ab =
    let link = dartOwner ab
        ba = opposite ab
        ac = nextCCW ab
        ca = opposite ac
        bd = nextCW ba
        db = opposite bd
        n = 1 + numberOfVertices link

        res = implode
            ( numberOfFreeLoops link
            , let opp' x | x == ac    = (n, 0)
                         | x == bd    = (n, 1)
                         | x == db    = (n, 2)
                         | x == ca    = (n, 3)
                         | otherwise  = endPair' x
              in map (\ v -> (map opp' $ outcomingDarts v, vertexCrossing v)) (allVertices link)
                     ++ [(map beginPair' [ac, bd, db, ca], cross)]
            )

        rc = let v = nthVertex res n
             in min (rootCode' (nthOutcomingDart v 0) R.ccw)
                    (rootCode' (nthOutcomingDart v 1) R.cw)

    in ((3, rc), res)


nextGeneration :: (Crossing a) => [a] -> EmbeddedLink a -> [EmbeddedLink a]
nextGeneration cross link =
    map snd $ nubBy ((==) `on` fst) $ do
        (rc, child) <- do
            c <- cross
            d <- allHalfEdges link
            p0 c d : [p1 c d | beginVertex d /= beginVertex (opposite d)
                            && opposite (nextCCW d) /= nextCW (opposite d)]

        let rc' = minimum [rootCode r dir | r <- allHalfEdges child, dir <- R.bothDirections]
        guard $ rc <= rc'
        return (rc, child)


rootCode :: (Crossing a) => Dart EmbeddedLink a -> R.RotationDirection -> (Int, UV.Vector Int)
rootCode ab dir | ac == opposite bd                         = (2, rootCode' ab dir)
                | nextDir dir (opposite ac) == opposite bd  = (3, rootCode' ab dir)
                | otherwise                                 = (4, UV.empty)
    where
        ba = opposite ab
        ac = nextDir dir ab
        bd = nextDir (R.oppositeDirection dir) ba


rootCode' :: (Crossing a) => Dart EmbeddedLink a -> R.RotationDirection -> UV.Vector Int
rootCode' root dir =
    case globalTransformations link of
        Nothing      -> codeWithGlobal D4.i
        Just globals -> minimum $ map codeWithGlobal globals
    where 
        link = dartOwner root
        n = numberOfVertices link

        codeWithGlobal global = UV.create $ do
                index <- UMV.replicate (n + 1) 0
                incoming <- UMV.replicate (n + 1) 0
                queue <- MV.new n
                free <- newSTRef 1

                let {-# INLINE look #-}
                    look !d = do
                        let u = beginVertexIndex d
                        ux <- UMV.unsafeRead index u
                        if ux > 0
                            then do
                                up <- UMV.unsafeRead incoming u
                                return $! (ux `shiftL` 2) + (((beginPlace d - up) * R.directionSign dir) .&. 3)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite index u nf
                                UMV.unsafeWrite incoming u (beginPlace d)
                                MV.unsafeWrite queue (nf - 1) d
                                return $! nf `shiftL` 2

                rc <- UMV.replicate (6 * n + 1) 0
                UMV.unsafeWrite rc 0 $! numberOfFreeLoops link

                let {-# INLINE lookAndWrite #-}
                    lookAndWrite !d !off = do
                        look d >>= UMV.unsafeWrite rc off
                        return $! off + 1

                void $ look root
                flip fix 0 $ \ bfs !headI -> do
                            tailI <- readSTRef free
                            when (headI < tailI - 1) $ do
                                input <- MV.unsafeRead queue headI
                                void $ foldMAdjacentDartsFrom input dir lookAndWrite (6 * headI + 3)
                                case crossingCodeWithGlobal global dir input of
                                    (# be, le #) -> do
                                        UMV.unsafeWrite rc (6 * headI + 1) be
                                        UMV.unsafeWrite rc (6 * headI + 2) le
                                bfs $! headI + 1

                fix $ \ _ -> do
                    tailI <- readSTRef free
                    when (tailI <= n) $
                        fail "codeWithDirection: disconnected diagram (not implemented)"

                return rc


main :: IO ()
main = do
    [maxN] <- fmap (map read) getArgs

    let projections =
            filter testPrime $
                tangleStarGlue
                    (filter ((== 1) . genusOfChordDiagram . fst) . listChordDiagrams . generateNonPlanarRaw)
                    (forCCP_ $ primeProjections maxN)

    let quads =
            sortBy (comparing numberOfVertices) $
                filter (\ link -> minimum (map faceDegree $ allFaces link) >= 4) projections

    let projections' = do
            root <- quads
            let walk link | numberOfVertices link >= maxN  = [link]
                          | otherwise                      = link : next
                    where
                        next = concatMap walk (nextGeneration projectionCrossings link)
            walk root

    printTable "Projections" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ projections)

    printTable "Quads" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ quads)

    printTable "Projections'" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ projections')

    renderCairo (printf "torus-links-quads-%i.svg" maxN) (Width 500) $ pad 1.05 $
        vcat' with { _sep = 0.5 } $ do
            linkGroup <- groupBy (on (==) numberOfVertices) quads
            return $ hcat' with { _sep = 0.4 } $ do
                link <- linkGroup
                return $ drawKnotDef link <> strutX 2 <> strutY 2
