module Math.Topology.KnotTh.Invariants.KhovanovHomology.PlanarAlgebra
    ( PlanarAlgebra'(..)
    , defaultGlueSubst
    ) where

import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV


class PlanarAlgebra' a where
    numberOfLegs :: a -> Int
    rotate       :: Int -> a -> a
    glue         :: Int -> (a, Int) -> (a, Int) -> (a, UV.Vector Int, UV.Vector Int)
--    glueBatch    :: Int -> (Int, Int) -> [(a, a)] -> ([a], UV.Vector Int, UV.Vector Int)
{-
    glue gl (a, ap) (b, bp) =
        let ([r], as, bs) = glueBatch gl (ap, bp) [(a, b)]
        in (a, as, bs)
-}


defaultGlueSubst :: Int -> (Int, Int) -> (Int, Int) -> (UV.Vector Int, UV.Vector Int)
defaultGlueSubst !gl (!nA, !posA) (!nB, !posB) =
    let substA = UV.create $ do
            s <- UMV.replicate nA (-1)
            forM_ [0 .. nA - gl - 1] $ \ !i ->
                UMV.write s ((posA + gl + i) `mod` nA) i
            forM_ [0 .. gl - 1] $ \ !i ->
                UMV.write s ((posA + gl - 1 - i) `mod` nA) $! -1 - ((posB + i) `mod` nB)
            return s

        substB = UV.create $ do
            s <- UMV.replicate nB (-1)
            forM_ [0 .. nB - gl - 1] $ \ !i ->
                UMV.write s ((posB + gl + i) `mod` nB) (nA - gl + i)
            forM_ [0 .. gl - 1] $ \ !i ->
                UMV.write s ((posB + gl - i - 1) `mod` nB) $! -1 - ((posA + i) `mod` nA)
            return s

    in substA `seq` substB `seq` (substA, substB)
