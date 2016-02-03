module Math.Topology.KnotTh.SurfaceGraph.Embedding.Optimization
    ( InteractionConst(..)
    , relaxEmbedding'
    , conjugateGradientSolve'
    , circlePacking'
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, void, liftM)
import Data.Array.Base (newArray, newArray_, newListArray, readArray, writeArray, getElems)
import Data.Array.Storable (StorableArray, withStorableArray)
import Foreign.C.Types
import qualified Foreign.Marshal.Array as A
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (with, withMany)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))


#include <Math/Topology/KnotTh/SurfaceGraph/Embedding/Optimization.h>


data InteractionConst = InteractionConst
    { interactionBorder   :: Double
    , interactionElectric :: Double
    , interactionBend     :: Double
    , interactionElastic  :: Double
    , interactionCross    :: Double
    }

{# pointer *InteractionConst as InteractionConstPtr -> InteractionConst #}

instance Storable InteractionConst where
    sizeOf _ = {# sizeof InteractionConst #}

    alignment _ = {# alignof InteractionConst #}

    peek p = InteractionConst
        <$> liftM realToFrac ({# get InteractionConst.border   #} p)
        <*> liftM realToFrac ({# get InteractionConst.electric #} p)
        <*> liftM realToFrac ({# get InteractionConst.bend     #} p)
        <*> liftM realToFrac ({# get InteractionConst.elastic  #} p)
        <*> liftM realToFrac ({# get InteractionConst.cross    #} p)

    poke p x = do
        {# set InteractionConst.border   #} p $ realToFrac $ interactionBorder x
        {# set InteractionConst.electric #} p $ realToFrac $ interactionElectric x
        {# set InteractionConst.bend     #} p $ realToFrac $ interactionBend x
        {# set InteractionConst.elastic  #} p $ realToFrac $ interactionElastic x
        {# set InteractionConst.cross    #} p $ realToFrac $ interactionCross x


foreign import ccall "relaxEmbedding"
    c_relaxEmbedding :: InteractionConstPtr -> CSize
        -> CSize -> CSize -> Ptr CDouble
        -> CSize -> Ptr CSize -> Ptr (Ptr CSize)
        -> CSize -> Ptr CSize -> Ptr (Ptr CSize)
        -> IO ()


relaxEmbedding' :: InteractionConst -> Int -> Int -> Int -> StorableArray Int CDouble -> [[Int]] -> [[Int]] -> IO ()
relaxEmbedding' interaction borderSegments numberOfMovablePoints numberOfFrozenPoints coords threads aliveVertices =
    with interaction $ \ interactionPtr ->
        withStorableArray coords $ \ xPtr -> do
            let numberOfThreads = length threads
            threadPtrs <- newListArray (0, numberOfThreads - 1) =<< mapM (A.newArray . map fromIntegral) threads

            let numberOfAliveVertices = length aliveVertices
            adjPtrs <- newListArray (0, numberOfAliveVertices - 1) =<< mapM (A.newArray . map fromIntegral) aliveVertices

            withStorableArray threadPtrs $ \ threadsPtrsPtr ->
                A.withArray (map (fromIntegral . length) threads) $ \ lensPtr ->
                    A.withArray (map (fromIntegral . length) aliveVertices) $ \ vertexDegreePtr ->
                        withStorableArray adjPtrs $ \ adjPtrsPtr ->
                            c_relaxEmbedding interactionPtr (fromIntegral borderSegments)
                                (fromIntegral numberOfMovablePoints) (fromIntegral numberOfFrozenPoints) xPtr
                                (fromIntegral numberOfThreads) lensPtr threadsPtrsPtr
                                (fromIntegral numberOfAliveVertices) vertexDegreePtr adjPtrsPtr

            getElems adjPtrs >>= mapM_ free
            getElems threadPtrs >>= mapM_ free


foreign import ccall "conjugateGradientSolve"
    c_conjugateGradientSolve :: CSize -> CSize -> Ptr CSize -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble


conjugateGradientSolve' :: Int -> [(Int, Int, Double)] -> [(Int, Double, Double)] -> (StorableArray Int CDouble, StorableArray Int CDouble) -> IO ()
conjugateGradientSolve' n defs brd (x, y) = do
    let m = length defs

    coords <- newArray_ (0, 2 * m - 1) :: IO (StorableArray Int CSize)
    a <- newArray_ (0, m - 1) :: IO (StorableArray Int CDouble)
    forM_ (zip [0 ..] defs) $ \ (!i, (!cx, !cy, !d)) -> do
        writeArray a i $ realToFrac d
        writeArray coords (2 * i) $ fromIntegral cx
        writeArray coords (2 * i + 1) $ fromIntegral cy

    bx <- newArray (0, n - 1) 0 :: IO (StorableArray Int CDouble)
    by <- newArray (0, n - 1) 0 :: IO (StorableArray Int CDouble)
    forM_ brd $ \ (!i, !cx, !cy) -> do
        readArray bx i >>= writeArray bx i . (+ realToFrac cx)
        readArray by i >>= writeArray by i . (+ realToFrac cy)

    withStorableArray coords $ \ pc ->
        withStorableArray a $ \ pa -> do
            withMany withStorableArray [bx, x] $ \ [pbx, px] ->
                void $ c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) pc pa pbx px

            withMany withStorableArray [by, y] $ \ [pby, py] ->
                void $ c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) pc pa pby py


foreign import ccall "circlePacking"
    c_circlePacking :: CSize -> Ptr CSize -> Ptr (Ptr CSize) -> Ptr CDouble -> IO ()


circlePacking' :: [[Int]] -> StorableArray Int CDouble -> IO ()
circlePacking' adj r =
    A.withArray (map (fromIntegral . length) adj) $ \ lensPtr -> do
        let n = length adj
        ls <- newListArray (0, n - 1) =<< mapM (A.newArray . map fromIntegral) adj
        withStorableArray ls $ \ lsPtr ->
            withStorableArray r $ \ rPtr ->
                c_circlePacking (fromIntegral n) lensPtr lsPtr rPtr
        getElems ls >>= mapM_ free
