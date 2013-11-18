{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.Topology.KnotTh.SurfaceLink.Definition.SurfaceLink
    ( SurfaceLink
    , emptySurfaceLink
    , changeNumberOfFreeLoops
    ) where

import Language.Haskell.TH
import Data.Function (fix)
import Data.Ix (Ix(..))
import Data.Bits ((.&.), complement)
import Data.Array.IArray (listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, foldM, foldM_)
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.TH.Knotted
import Math.Topology.KnotTh.Knotted.TH.Show


produceKnotted
    [d| data SurfaceLink ct = SurfaceLink
            { faceCount      :: !Int
            , faceDataOffset :: !(UArray Int Int)
            , faceCCWBrdDart :: !(UArray Int Int)
            , faceLLookup    :: !(UArray Int Int)
            }
    |] $
    let fcN = mkName "fc"
        fllookN = mkName "fllook"
        foffN = mkName "foff"
        fccwdN = mkName "fccwd"
    in defaultKnotted
        { implodeExplodeSettings = defaultImplodeExplode
            { implodePostExtra = \ n cr spliceFill -> (:[]) $
                bindS (tupP [varP fcN, varP fllookN, varP foffN, varP fccwdN]) [| do
                    fccwd <- newArray_ (0, 4 * $n - 1) :: ST s (STUArray s Int Int)
                    fllook <- newArray (0, 8 * $n - 1) (-1) :: ST s (STUArray s Int Int)

                    (fc, _) <- foldM (\ (!fid, !base) !start -> do
                        mi <- readArray fllook (2 * start)
                        if mi >= 0
                            then return (fid, base)
                            else do
                                sz <- fix (\ mark !offset !i -> do
                                    writeArray fllook (2 * i) fid
                                    writeArray fllook (2 * i + 1) offset
                                    writeArray fccwd (base + offset) i

                                    i' <- readArray $cr i
                                    let j = (i' .&. complement 3) + ((i' - 1) .&. 3)
                                    mj <- readArray fllook (2 * j)
                                    if mj >= 0
                                        then return $! offset + 1
                                        else mark (offset + 1) j
                                    ) 0 start
                                return (fid + 1, base + sz)
                        ) (0, 0) [0 .. 4 * $n - 1]

                    foff <- newArray (0, fc) 0 :: ST s (STUArray s Int Int)
                    forM_ [0 .. 4 * $n - 1] $ \ !i -> do
                        fid <- readArray fllook (2 * i)
                        cur <- readArray foff fid
                        writeArray foff fid $! cur + 1
                    foldM_ (\ !offset !i -> do
                        cur <- readArray foff i
                        writeArray foff i offset
                        return $! offset + cur
                        ) 0 [0 .. fc]

                    fccwd' <- unsafeFreeze fccwd
                    fllook' <- unsafeFreeze fllook
                    foff' <- unsafeFreeze foff
                    return (fc, fllook', foff', fccwd')
                    |]

            , implodeInitializers =
                [ (,) (mkName "faceCount")      `fmap` varE fcN
                , (,) (mkName "faceDataOffset") `fmap` varE foffN
                , (,) (mkName "faceCCWBrdDart") `fmap` varE fccwdN
                , (,) (mkName "faceLLookup")    `fmap` varE fllookN
                ]
            }

        , emptyExtraInitializers =
            [ (,) (mkName "faceCount")      `fmap` [| 1 :: Int |]
            , (,) (mkName "faceDataOffset") `fmap` [| listArray (0 :: Int, 1) [0, 0] |]
            , (,) (mkName "faceCCWBrdDart") `fmap` [| listArray (0 :: Int, -1) [] |]
            , (,) (mkName "faceLLookup")    `fmap` [| listArray (0 :: Int, -1) [] |]
            ]
        }

produceShowDart ''SurfaceLink ''Dart (const [])
produceShowCrossing ''SurfaceLink ''Vertex
produceShowKnot ''SurfaceLink


instance SurfaceDiagram SurfaceLink where
    numberOfFaces = faceCount

    nthFace link i | i > 0 && i <= n  = Face link (i - 1)
                   | otherwise        = error $ printf "nthFace: index %i is out of bounds (1, %i)" i n
        where
            n = numberOfFaces link

    allFaces link = map (Face link) [1 .. numberOfFaces link]

    data Face SurfaceLink ct = Face !(SurfaceLink ct) {-# UNPACK #-} !Int

    faceDegree (Face l i) =
        let cur = faceDataOffset l `unsafeAt` i
            nxt = faceDataOffset l `unsafeAt` (i + 1)
        in nxt - cur

    faceOwner = faceSurfaceLink

    faceIndex (Face _ i) = i + 1

    leftFace (Dart l i) = Face l $ faceLLookup l `unsafeAt` (2 * i)

    leftPlace (Dart l i) = faceLLookup l `unsafeAt` (2 * i + 1)

    nthDartInCCWTraverse (Face l i) p =
        let cur = faceDataOffset l `unsafeAt` i
            nxt = faceDataOffset l `unsafeAt` (i + 1)
        in Dart l $ faceCCWBrdDart l `unsafeAt` (cur + p `mod` (nxt - cur))

    faceIndicesRange l = (1, numberOfFaces l)


instance SurfaceKnotted SurfaceLink


instance Eq (Face SurfaceLink ct) where
    (==) (Face _ a) (Face _ b) = a == b


instance Ord (Face SurfaceLink ct) where
    compare (Face _ a) (Face _ b) = compare a b


instance Ix (Face SurfaceLink ct) where
    range (Face _ a, Face l b) = map (Face l) [a .. b]

    rangeSize (Face _ a, Face _ b) = max 0 $ 1 + b - a

    inRange (Face _ a, Face _ b) (Face _ i) = (i >= a) && (i <= b)

    index (Face _ a, Face _ b) (Face _ i)
        | (i >= a) && (i <= b)  = i - a
        | otherwise             = error "out of range"


{-# INLINE faceSurfaceLink #-}
faceSurfaceLink :: Face SurfaceLink ct -> SurfaceLink ct
faceSurfaceLink (Face l _) = l
