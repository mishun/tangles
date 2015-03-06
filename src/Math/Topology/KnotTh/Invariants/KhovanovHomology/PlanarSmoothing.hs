module Math.Topology.KnotTh.Invariants.KhovanovHomology.PlanarSmoothing
    ( PlanarAlgebra'(..)
    , PlanarSmoothing
    , numberOfLoops
    ) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.Topology.KnotTh.Invariants.KhovanovHomology.PlanarAlgebra


data PlanarSmoothing = Smoothing {-# UNPACK #-} !Int !(UV.Vector Int)
    deriving (Eq, Ord)


numberOfLoops :: PlanarSmoothing -> Int
numberOfLoops (Smoothing ls _) = ls


instance PlanarAlgebra' PlanarSmoothing where
    numberOfLegs (Smoothing _ a) = UV.length a

    rotate _ x = x

    glue !gl (Smoothing loopsA a, !posA) (Smoothing loopsB b, !posB) =
        let nA = UV.length a
            substA = UV.create $ do
                s <- UMV.replicate nA (-1)
                forM_ [0 .. nA - gl - 1] $ \ !i ->
                    UMV.write s ((posA + gl + i) `mod` nA) i
                forM_ [0 .. gl - 1] $ \ !i ->
                    UMV.write s ((posA + gl - 1 - i) `mod` nA) $! -1 - ((posB + i) `mod` nB)
                return s

            nB = UV.length b
            substB = UV.create $ do
                s <- UMV.replicate nB (-1)
                forM_ [0 .. nB - gl - 1] $ \ !i ->
                    UMV.write s ((posB + gl + i) `mod` nB) (nA - gl + i)
                forM_ [0 .. gl - 1] $ \ !i ->
                    UMV.write s ((posB + gl - i - 1) `mod` nB) $! -1 - ((posA + i) `mod` nA)
                return s

            res = UV.create $ do
                let mateA x | ys >= 0    = ys
                            | otherwise  = mateB $! -1 - ys
                        where ys = substA UV.! (a UV.! x)

                    mateB x | ys >= 0    = ys
                            | otherwise  = mateA $! -1 - ys
                        where ys = substB UV.! (b UV.! x)

                r <- UMV.new (nA - gl + nB - gl)
                forM_ [0 .. nA - gl - 1] $ \ !i ->
                    UMV.write r i $! mateA $! (posA + gl + i) `mod` nA
                forM_ [0 .. nB - gl - 1] $ \ !i ->
                    UMV.write r (nA - gl + i) $! mateB $! (posB + gl + i) `mod` nB
                return r

            loops = runST $ do
                vis <- UMV.replicate nA False

                let goUpA target p | p == target  = return True
                                   | otherwise    = do
                        visited <- UMV.read vis p
                        if visited
                            then return False
                            else UMV.write vis p True >> (goDownA target $! a UV.! p)

                    goDownA target p | p' >= 0    = return False
                                     | otherwise  = UMV.write vis p True >> (goUpB target $! -1 - p')
                        where p' = substA UV.! p

                    goUpB target p = goDownB target $! b UV.! p

                    goDownB target p | p' >= 0    = return False
                                     | otherwise  = goUpA target $! -1 - p'
                        where p' = substB UV.! p

                foldM (\ !loopsCount !i -> do
                        let p = (posA + i) `mod` nA
                        visited <- UMV.read vis p
                        if visited
                            then return loopsCount
                            else do
                                UMV.write vis p True
                                isLoop <- goDownA p
                                 $! a UV.! p
                                return $! if isLoop then loopsCount + 1
                                                    else loopsCount
                    )
                    (loopsA + loopsB)
                    [0 .. gl - 1]

        in (Smoothing loops res, substA, substB)
