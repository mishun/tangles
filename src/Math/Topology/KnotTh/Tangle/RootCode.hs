{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.RootCode
    ( totalRootCode
    ) where

import Control.Monad (foldM, forM_, void, when)
import qualified Control.Monad.ST as ST
import Data.Bits (complement, shiftL, shiftR, (.&.))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Primitive as PV
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted


totalRootCode :: (Crossing a) => Int -> Int -> Int -> Maybe [D4] -> PV.Vector Int -> V.Vector a -> UV.Vector Int
totalRootCode n legs loops global opp cross = UV.concat $ UV.singleton loops : UV.singleton legs : border : internal
    where
        baseL = 4 * n

        border | legs == 0  = UV.empty
               | otherwise  = minimum $ do
            baseLeg <- [0 .. legs - 1]
            dir <- bothDirections
            globalG <- fromMaybe [d4I] global

            return $! UV.create $ do
                index <- UMV.replicate (n + 1) 0
                incoming <- UMV.replicate (n + 1) 0
                queue <- UMV.new n
                free <- newSTRef 1

                let {-# INLINE look #-}
                    look !d | d  >= baseL  =
                                let offset = (d - baseLeg - baseL) * directionSign dir
                                in return $! -(offset `mod` legs)

                            | otherwise    = do
                                let u = d `shiftR` 2
                                    p = d .&. 3
                                ux <- UMV.read index u
                                if ux > 0
                                    then do
                                        base <- UMV.unsafeRead incoming u
                                        return $! (ux `shiftL` 2) + (((p - base) * directionSign dir) .&. 3)
                                    else do
                                        nf <- readSTRef free
                                        writeSTRef free $! nf + 1
                                        UMV.write index u nf
                                        UMV.unsafeWrite incoming u p
                                        UMV.write queue (nf - 1) d
                                        return $! nf `shiftL` 2

                rc <- UMV.new (legs + 5 * n)
                forM_ [0 .. legs - 1] $ \ !i ->
                    let d = baseL + (baseLeg + directionSign dir * i) `mod` legs
                    in look (opp PV.! d) >>= UMV.write rc i

                let bfs !headI !offset = do
                        tailI <- readSTRef free
                        if headI >= tailI - 1
                            then return $ UMV.take offset rc
                            else do
                                input <- UMV.read queue headI

                                UMV.write rc offset $!
                                    crossingCodeWithGlobal' (cross V.! (input `shiftR` 2)) globalG dir input

                                forM_ [0 .. 3] $ \ !i -> do
                                    let d = (input .&. complement 3) + ((input + directionSign dir * i) .&. 3)
                                    look (opp PV.! d) >>= UMV.write rc (offset + 1 + i)

                                bfs (headI + 1) (offset + 5)

                bfs 0 legs

        internal | UV.length border >= (legs + 2 * n)  = []
                 | otherwise                           = sort $ map compCode comps
            where
                comps = ST.runST $ do
                    visited <- UMV.replicate n (-1)
                    let dfs mark v = do
                            vis <- UMV.read visited v
                            when (vis < 0) $ do
                                UMV.write visited v mark
                                forM_ [4 * v .. 4 * v + 3] $ \ !i ->
                                    let d = opp PV.! i
                                    in when (d < baseL) (dfs mark $ d `shiftR` 2)

                    forM_ [0 .. legs - 1] $ \ !i ->
                        let d = opp PV.! (baseL + i)
                        in when (d < baseL) (dfs 0 $ d `shiftL` 2)

                    c <- foldM (\ !ci !v -> do
                            vis <- UMV.read visited v
                            if vis < 0
                                then dfs ci v >> return (ci + 1)
                                else return ci
                        ) 1 [0 .. n - 1]

                    lists <- MV.replicate c []
                    forM_ [0 .. n - 1] $ \ !v ->
                        UMV.read visited v >>= MV.modify lists (v :)

                    mapM (MV.read lists) [1 .. c - 1]

                compCode comp = minimum $ do
                    start <- comp
                    root <- [4 * start .. 4 * start + 3]
                    dir <- bothDirections
                    globalG <- fromMaybe [d4I] global

                    return $! UV.create $ do
                        index <- UMV.replicate (n + 1) 0
                        incoming <- UMV.replicate (n + 1) 0
                        queue <- UMV.new n
                        free <- newSTRef 1

                        let {-# INLINE look #-}
                            look !d = do
                                let u = d `shiftR` 2
                                    p = d .&.3
                                ux <- UMV.read index u
                                if ux > 0
                                    then do
                                        base <- UMV.unsafeRead incoming u
                                        return $! (ux `shiftL` 2) + (((p - base) * directionSign dir) .&. 3)
                                    else do
                                        nf <- readSTRef free
                                        writeSTRef free $! nf + 1
                                        UMV.write index u nf
                                        UMV.unsafeWrite incoming u p
                                        UMV.write queue (nf - 1) d
                                        return $! nf `shiftL` 2

                        rc <- UMV.new (5 * n)
                        void $ look root

                        let bfs !headI !offset = do
                                tailI <- readSTRef free
                                if headI >= tailI - 1
                                    then return $! UMV.take offset rc
                                    else do
                                        input <- UMV.read queue headI

                                        UMV.write rc offset $!
                                            crossingCodeWithGlobal' (cross V.! (input `shiftR` 2)) globalG dir input

                                        forM_ [0 .. 3] $ \ !i -> do
                                            let d = (input .&. complement 3) + ((input + directionSign dir * i) .&. 3)
                                            look (opp PV.! d) >>= UMV.write rc (offset + 1 + i)

                                        bfs (headI + 1) (offset + 5)

                        bfs 0 0
