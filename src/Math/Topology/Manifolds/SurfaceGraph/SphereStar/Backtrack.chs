module Math.Topology.Manifolds.SurfaceGraph.SphereStar.Backtrack
    ( backtrack
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray)
import Data.Array.IO (IOUArray)
import Data.Array.Storable (withStorableArray)
import Control.Monad (forM_, liftM2)
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Math.Topology.Manifolds.SurfaceGraph.Definition
import Math.Topology.Manifolds.SurfaceGraph.Util


foreign import ccall "sphereStarDecomposition"
    c_sphereStarDecomposition
        :: CSize -> CSize -> CSize
            -> Ptr CSize -> Ptr CSize -> Ptr CSize
                -> Ptr CSize -> Ptr CSize -> IO ()


backtrack :: SurfaceGraph -> (UArray Face Bool, UArray Dart Bool)
backtrack graph = unsafePerformIO $ do
    let v = numberOfVertices graph
        f = numberOfFaces graph
        e = numberOfEdges graph

    edges <- newArray_ (0, 2 * e - 1)
    coedges <- newArray_ (0, 2 * e - 1)
    forM_ ([0, 2 ..] `zip` graphEdges graph) $ \ (offset, (a, b)) -> do
        writeArray edges (offset + 0) (fromIntegral $ vertexIndex $ beginVertex a)
        writeArray edges (offset + 1) (fromIntegral $ vertexIndex $ beginVertex b)
        writeArray coedges (offset + 0) (fromIntegral $ faceIndex $ leftFace a)
        writeArray coedges (offset + 1) (fromIntegral $ faceIndex $ leftFace b)
        
    vertexDegrees <- newArray_ (0, v - 1)
    forM_ ([0 ..] `zip` graphVertices graph) $ \ (offset, ver) -> do
        writeArray vertexDegrees offset (fromIntegral $ vertexDegree ver)

    resultFaces <- newArray_ (0, f - 1)
    resultEdges <- newArray_ (0, e - 1)
    withStorableArray vertexDegrees $ \ vertexDegreesPtr ->
        withStorableArray edges $ \ edgesPtr ->
            withStorableArray coedges $ \ coedgesPtr ->
                withStorableArray resultFaces $ \ resultFacesPtr ->
                    withStorableArray resultEdges $ \ resultEdgesPtr ->
                        c_sphereStarDecomposition
                            (fromIntegral v) (fromIntegral f) (fromIntegral e)
                            vertexDegreesPtr edgesPtr coedgesPtr
                            resultFacesPtr resultEdgesPtr

    faceMarks <- newArray (facesRange graph) False :: IO (IOUArray Face Bool)
    forM_ ([0 ..] `zip` graphFaces graph) $ \ (i, face) -> do
        mark <- (/= 0) `fmap` readArray resultFaces i
        writeArray faceMarks face mark

    edgeMarks <- newArray (dartsRange graph) False :: IO (IOUArray Dart Bool)
    forM_ ([0 ..] `zip` graphEdges graph) $ \ (i, (a, b)) -> do
        mark <- (/= 0) `fmap` readArray resultEdges i
        writeArray edgeMarks a mark
        writeArray edgeMarks b mark

    liftM2 (,) (freeze faceMarks) (freeze edgeMarks)
