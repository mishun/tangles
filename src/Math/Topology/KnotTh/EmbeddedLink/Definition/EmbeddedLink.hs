{-# LANGUAGE TemplateHaskell, TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.EmbeddedLink.Definition.EmbeddedLink
    ( EmbeddedLink
    , EmbeddedLinkProjection
    , EmbeddedLinkProjectionVertex
    , EmbeddedLinkProjectionDart
    , EmbeddedLinkDiagram
    , EmbeddedLinkDiagramVertex
    , EmbeddedLinkDiagramDart
    ) where

import Language.Haskell.TH
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Bits ((.&.), shiftL, complement)
import Data.Array.IArray (listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STArray, STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST)
import Control.Monad (void, when, forM_, foldM, foldM_)
import Text.Printf
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Knotted.TH.Knotted
import Math.Topology.KnotTh.Knotted.TH.Show


produceKnotted
    [d| data EmbeddedLink ct =
            EmbeddedLink
                { faceCount      :: !Int
                , faceDataOffset :: !(UArray Int Int)
                , faceCCWBrdDart :: !(UArray Int Int)
                , faceLLookup    :: !(UArray Int Int)
                }

        instance Knotted EmbeddedLink where
            vertexCrossing = undefined
            numberOfFreeLoops = undefined
            changeNumberOfFreeLoops = undefined
            emptyKnotted = undefined

            type ExplodeType EmbeddedLink a = (Int, [([(Int, Int)], a)])

            implode = undefined

            explode link =
                ( numberOfFreeLoops link
                , map (\ v -> (map endPair' $ outcomingDarts v, vertexCrossing v)) $ allVertices link
                )

            homeomorphismInvariant link =
                minimum $ do
                    dart <- allHalfEdges link
                    dir <- R.bothDirections
                    globalG <- fromMaybe [D4.i] $ globalTransformations link
                    return $! codeWithDirection globalG dir dart

                where
                    codeWithDirection !globalG !dir !start = runSTUArray $ do
                        let n = numberOfVertices link

                        index <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
                        incoming <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
                        queue <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart EmbeddedLink ct))
                        free <- newSTRef 1

                        let {-# INLINE look #-}
                            look !d = do
                                let u = beginVertexIndex d
                                ux <- unsafeRead index u
                                if ux > 0
                                    then do
                                        up <- unsafeRead incoming u
                                        return $! (ux `shiftL` 2) + (((beginPlace d - up) * R.directionSign dir) .&. 3)
                                    else do
                                        nf <- readSTRef free
                                        writeSTRef free $! nf + 1
                                        unsafeWrite index u nf
                                        unsafeWrite incoming u (beginPlace d)
                                        unsafeWrite queue (nf - 1) d
                                        return $! nf `shiftL` 2

                        rc <- newArray (0, 6 * n) 0 :: ST s (STUArray s Int Int)
                        unsafeWrite rc 0 $! numberOfFreeLoops link

                        let {-# INLINE lookAndWrite #-}
                            lookAndWrite !d !offset = do
                                look d >>= unsafeWrite rc offset
                                return $! offset + 1

                        void $ look start
                        flip fix 0 $ \ bfs !headI -> do
                            tailI <- readSTRef free
                            when (headI < tailI - 1) $ do
                                input <- unsafeRead queue headI
                                void $ foldMAdjacentDartsFrom input dir lookAndWrite (6 * headI + 3)
                                case crossingCodeWithGlobal globalG dir input of
                                    (# be, le #) -> do
                                        unsafeWrite rc (6 * headI + 1) be
                                        unsafeWrite rc (6 * headI + 2) le
                                bfs $! headI + 1

                        fix $ \ _ -> do
                            tailI <- readSTRef free
                            when (tailI <= n) $
                                fail "codeWithDirection: disconnected diagram (not implemented)"

                        return rc

            isConnected _ = error "isConnected: not implemented"

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


produceShowDart ''EmbeddedLink (const [])
produceShowVertex ''EmbeddedLink
produceShowKnotted ''EmbeddedLink


instance SurfaceDiagram EmbeddedLink where
    numberOfFaces = faceCount

    nthFace link i | i > 0 && i <= n  = Face link (i - 1)
                   | otherwise        = error $ printf "nthFace: index %i is out of bounds (1, %i)" i n
        where
            n = numberOfFaces link

    allFaces link = map (Face link) [1 .. numberOfFaces link]

    data Face EmbeddedLink ct = Face !(EmbeddedLink ct) {-# UNPACK #-} !Int

    faceDegree (Face l i) =
        let cur = faceDataOffset l `unsafeAt` i
            nxt = faceDataOffset l `unsafeAt` (i + 1)
        in nxt - cur

    faceOwner (Face l _) = l

    faceIndex (Face _ i) = i + 1

    leftFace (Dart l i) = Face l $ faceLLookup l `unsafeAt` (2 * i)

    leftPlace (Dart l i) = faceLLookup l `unsafeAt` (2 * i + 1)

    nthDartInCCWTraverse (Face l i) p =
        let cur = faceDataOffset l `unsafeAt` i
            nxt = faceDataOffset l `unsafeAt` (i + 1)
        in Dart l $ faceCCWBrdDart l `unsafeAt` (cur + p `mod` (nxt - cur))

    faceIndicesRange l = (1, numberOfFaces l)


instance SurfaceKnotted EmbeddedLink


type EmbeddedLinkProjection = EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjectionVertex = Vertex EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjectionDart = Dart EmbeddedLink ProjectionCrossing


type EmbeddedLinkDiagram = EmbeddedLink DiagramCrossing
type EmbeddedLinkDiagramVertex = Vertex EmbeddedLink DiagramCrossing
type EmbeddedLinkDiagramDart = Dart EmbeddedLink DiagramCrossing
