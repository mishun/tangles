{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Tabulation.LinkDiagrams
    ( nextGeneration
    ) where

import Control.Monad (guard, when)
import Data.Bits (shiftL)
import Data.Function (on)
import Data.List (nubBy)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.Topology.KnotTh.Dihedral.D4
import Math.Topology.KnotTh.Link


p0 :: (Crossing a) => a -> Dart Link a -> ((Int, UV.Vector Int), Link a)
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
             in min (rootCode' (nthOutcomingDart v 3) ccw)
                    (rootCode' (nthOutcomingDart v 0) cw)

    in ((2, rc), res)


p1 :: (Crossing a) => a -> Dart Link a -> ((Int, UV.Vector Int), Link a)
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
             in min (rootCode' (nthOutcomingDart v 0) ccw)
                    (rootCode' (nthOutcomingDart v 1) cw)

    in ((3, rc), res)


nextGeneration :: (Crossing a) => [a] -> Link a -> [Link a]
nextGeneration cross link =
    map snd $ nubBy ((==) `on` fst) $ do
        (rc, child) <- do
            c <- cross
            d <- allDarts link
            p0 c d : [p1 c d | opposite (nextCCW d) /= nextCW (opposite d)]

        let rc' = minimum [rootCode r dir | r <- allDarts child, dir <- bothDirections]
        guard $ rc <= rc'
        return (rc, child)


rootCode :: (Crossing a) => Dart Link a -> RotationDirection -> (Int, UV.Vector Int)
rootCode ab dir | ac == opposite bd                         = (2, rootCode' ab dir)
                | nextDir dir (opposite ac) == opposite bd  = (3, rootCode' ab dir)
                | otherwise                                 = (4, UV.empty)
    where
        ba = opposite ab
        ac = nextDir dir ab
        bd = nextDir (mirrorIt dir) ba


rootCode' :: (Crossing a) => Dart Link a -> RotationDirection -> UV.Vector Int
rootCode' root dir =
    case globalTransformations link of
        Nothing      -> codeWithGlobal d4I
        Just globals -> minimum $ map codeWithGlobal globals
    where
        link = dartOwner root
        n = numberOfVertices link

        codeWithGlobal global = UV.create $ do
            x <- UMV.replicate (n + 1) 0
            UMV.unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- MV.new n
            MV.unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s = do
                    let u = beginVertex d
                    ux <- UMV.unsafeRead x (vertexIndex u)
                    if ux > 0
                        then return $! ux + (s `shiftL` 7)
                        else do
                            nf <- readSTRef free
                            writeSTRef free $! nf + 1
                            UMV.unsafeWrite x (vertexIndex u) nf
                            MV.unsafeWrite q (nf - 1) d
                            return $! nf + (s `shiftL` 7)

            rc <- UMV.replicate (2 * n) 0

            let {-# INLINE bfs #-}
                bfs !h = when (h < n) $ do
                    d <- MV.unsafeRead q h
                    nb <- foldMIncomingDartsFrom d dir look 0
                    case crossingCodeWithGlobal global dir d of
                        (# be, le #) -> do
                            UMV.unsafeWrite rc (2 * h) be
                            UMV.unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
                    bfs $! h + 1

            bfs 0
            return rc
