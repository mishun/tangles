{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.SurfaceLink
    ( module Math.KnotTh.Knotted
    , SurfaceLink
    , Crossing
    , Face
    , Dart
    , crossingSurfaceLink
    , faceSurfaceLink
    , dartSurfaceLink
    , changeNumberOfFreeLoops
    , implode
    , explode
    , testPrime
    ) where

import Language.Haskell.TH
import Data.Function (fix)
import Data.Array.ST (STUArray, newArray, newArray_, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_, liftM)
import Text.Printf
import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Knotted
import Math.KnotTh.Knotted.TH.Show


produceKnotted
    [d|
        data SurfaceLink ct = SurfaceLink
            { faceCount :: Int
            }
    |]
    defaultKnotted
        { implodeExplodeSettings = Just $ defaultImplodeExplode
            { implodeInitializers = [liftM ((,) (mkName "faceCount")) $ litE (integerL 0)]
            }
        }

produceShowDart ''Dart
produceShowCrossing ''Crossing
produceShowKnot ''SurfaceLink


instance KnottedWithToPair SurfaceLink Crossing Dart


data Face ct = Face !(SurfaceLink ct) {-# UNPACK #-} !Int


{-# INLINE faceSurfaceLink #-}
faceSurfaceLink :: Face ct -> SurfaceLink ct
faceSurfaceLink (Face l _) = l


instance SurfaceKnotted SurfaceLink Crossing Face Dart where
    numberOfFaces = faceCount

    nthFace link i
        | i < 1 || i > numberOfFaces link  = error $ printf "nthFace: index %i is out of bounds (1, %i)" i (numberOfFaces link)
        | otherwise                        = Face link (i - 1)

    faceOwner = faceSurfaceLink

    faceIndex (Face _ i) = i + 1

    faceDegree = undefined


testPrime :: SurfaceLink ct -> Bool
testPrime link = runST $ do
    let sz = numberOfCrossings link
    g <- newArray ((1, 1), (sz, sz)) 0 :: ST s (STUArray s (Int, Int) Int)

    forM_ (allCrossings link) $ \ u ->
        forM_ (adjacentCrossings u) $ \ v -> do
            let i = (crossingIndex u, crossingIndex v)
            w <- readArray g i
            writeArray g i $! w + 1

    v <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    forM_ [1 .. sz] $ \ i ->
        writeArray v i i

    a <- newArray_ (1, sz) :: ST s (STUArray s Int Bool)
    w <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    na <- newArray_ (1, sz) :: ST s (STUArray s Int Int)

    let setA i x = do
            j <- readArray v i
            writeArray a j x

    flip fix sz $ \ loop !n -> do
        setA 1 True
        forM_ [2 .. n] $ \ i -> do
            setA i False
            writeArray na (i - 1) i
            writeArray w i =<< do
                p <- readArray v 1
                q <- readArray v i
                readArray g (p, q)

        when (n > 1) $ loop (n - 1)

    return True
