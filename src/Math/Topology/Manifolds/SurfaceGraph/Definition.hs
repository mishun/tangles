{-# LANGUAGE TypeFamilies #-}
module Math.Topology.Manifolds.SurfaceGraph.Definition
    ( module X
    , SurfaceGraph
    , dual
    , constructFromList
    ) where

import Data.Ix (Ix(..))
import Data.List (intercalate)
import Data.Array.IArray ((!), bounds, listArray, assocs, indices)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, foldM)
import Text.Printf
import Math.Algebra.PlanarAlgebra as X


data SurfaceGraph a =
    Graph
        { _opposite   :: {-# UNPACK #-} !(UArray Int Int)
        , _vertices   :: {-# UNPACK #-} !(Array Int (UArray Int Int))
        , _connToVert :: {-# UNPACK #-} !(Array Int (Int, Int))
        , _faces      :: {-# UNPACK #-} !(Array Int (UArray Int Int))
        , _connToFace :: {-# UNPACK #-} !(Array Int (Int, Int))
        }


instance PlanarDiagram SurfaceGraph where
    numberOfVertices g = (+ 1) $ snd $ bounds $ _vertices g

    numberOfEdges g = (`div` 2) $ (+ 1) $ snd $ bounds $ _opposite g

    nthVertex g i | inRange b i  = Vertex g i
                  | otherwise    = error $ printf "nthVertex: index %i is out of bounds %s" i (show b)
        where
            b = bounds (_vertices g)

    nthDart g i | inRange b i  = Dart g i
                | otherwise    = error $ printf "nthDart: index %i is out of bounds %s" i (show b)
        where
            b = bounds (_opposite g)

    allVertices g = map (Vertex g) $ range $ bounds $ _vertices g

    allHalfEdges g = map (nthDart g) [0 .. numberOfDarts g - 1]

    data Vertex SurfaceGraph a = Vertex !(SurfaceGraph a) {-# UNPACK #-} !Int

    vertexDegree (Vertex g i) = (+ 1) $ snd $ bounds (_vertices g ! i)

    vertexOwner (Vertex g _) = g

    vertexIndex (Vertex _ i) = i

    nthOutcomingDart v@(Vertex g i) j =
        let jj = j `mod` vertexDegree v
        in Dart g $ (_vertices g ! i) ! jj

    outcomingDarts v = map (nthOutcomingDart v) [0 .. vertexDegree v - 1]

    data Dart SurfaceGraph a = Dart !(SurfaceGraph a) {-# UNPACK #-} !Int

    dartOwner (Dart g _) = g

    dartIndex (Dart _ i) = i

    beginPair (Dart g i) =
        let (vi, p) = _connToVert g ! i
        in (Vertex g vi, p)

    opposite (Dart g i) = Dart g (_opposite g ! i)

    isDart _ = True


instance SurfaceDiagram SurfaceGraph where
    numberOfFaces g = (+ 1) $ snd $ bounds $ _faces g

    nthFace g i | inRange b i  = Face g i
                | otherwise    = error $ printf "nthFace: index %i is out of bounds %s" i (show b)
        where
            b = bounds (_faces g)

    allFaces g = map (Face g) $ range $ bounds $ _faces g

    data Face SurfaceGraph a = Face !(SurfaceGraph a) {-# UNPACK #-} !Int

    faceDegree (Face g i) = (+ 1) $ snd $ bounds (_faces g ! i)

    faceOwner (Face g _) = g

    faceIndex (Face _ i) = i

    leftPair (Dart g i) =
        let (fi, p) = _connToFace g ! i
        in (Face g fi, p)

    nthDartInCCWTraverse f@(Face g i) j =
        let jj = j `mod` faceDegree f
        in Dart g $ (_faces g ! i) ! jj


instance Eq (Vertex SurfaceGraph a) where
    (==) (Vertex _ a) (Vertex _ b) = a == b

instance Ord (Vertex SurfaceGraph a) where
    compare (Vertex _ a) (Vertex _ b) = compare a b

instance Ix (Vertex SurfaceGraph a) where
    range (Vertex _ a, Vertex g b) = map (Vertex g) [a .. b]
    index (Vertex _ a, Vertex _ b) (Vertex _ c) = index (a, b) c
    inRange (Vertex _ a, Vertex _ b) (Vertex _ c) = c >= a && c <= b
    rangeSize (Vertex _ a, Vertex _ b) = max 0 $ b - a + 1


instance Eq (Dart SurfaceGraph a) where
    (==) (Dart _ a) (Dart _ b) = a == b

instance Ord (Dart SurfaceGraph a) where
    compare (Dart _ a) (Dart _ b) = compare a b

instance Ix (Dart SurfaceGraph a) where
    range (Dart _ a, Dart g b) = map (Dart g) [a .. b]
    index (Dart _ a, Dart _ b) (Dart _ c) = index (a, b) c
    inRange (Dart _ a, Dart _ b) (Dart _ c) = c >= a && c <= b
    rangeSize (Dart _ a, Dart _ b) = max 0 $ b - a + 1


instance Eq (Face SurfaceGraph a) where
    (==) (Face _ a) (Face _ b) = a == b

instance Ord (Face SurfaceGraph a) where
    compare (Face _ a) (Face _ b) = compare a b

instance Ix (Face SurfaceGraph a) where
    range (Face _ a, Face g b) = map (Face g) [a .. b]
    index (Face _ a, Face _ b) (Face _ c) = index (a, b) c
    inRange (Face _ a, Face _ b) (Face _ c) = c >= a && c <= b
    rangeSize (Face _ a, Face _ b) = max 0 $ b - a + 1


instance Show (SurfaceGraph a) where
    show graph =
        let showVertex v =
                let inc = intercalate ", " $ map (show . beginPair') $ incomingDarts v
                in concat [show $ vertexIndex v, " -> {", inc, "}"]
        in (\ s -> concat ["{\n", s, "\n}"]) $ unlines $ map showVertex $ allVertices graph


dual :: SurfaceGraph a -> SurfaceGraph a
dual g =
    Graph
        { _opposite   = _opposite g
        , _vertices   = _faces g
        , _connToVert = _connToFace g
        , _faces      = _vertices g
        , _connToFace = _connToVert g
        }


constructFromList :: [[(Int, Int)]] -> SurfaceGraph a
constructFromList g
    | not idempotent  = error "constructFromList: bad connections"
    | otherwise       = completeDefinition opp vert
    where
        n = length g
        s = listArray (0, n - 1) $ map length g :: UArray Int Int
        offset = listArray (0, n) $ scanl (\ k i -> k + s ! i) 0 [0 .. n - 1] :: UArray Int Int

        indexD (v, p)
            | v < 0 || v >= n        = error $ printf "constructFromList: vertex index %i is out of bound" v
            | p < 0 || p >= (s ! v)  = error $ printf "constructFromList: dart index %i is out of bound" p
            | otherwise              = (offset ! v) + p

        opp = listArray (0, (offset ! n) - 1) $ map indexD $ concat g

        idempotent = all (\ (i, j) -> (i /= j) && (i == opp ! j)) $ assocs opp

        vert = listArray (0, n - 1) $ map (\ i ->
                let m = s ! i
                    o = offset ! i
                in listArray (0, m - 1) $ map (+ o) [0 .. m - 1]
            ) [0 .. n - 1]


completeDefinition :: UArray Int Int -> Array Int (UArray Int Int) -> SurfaceGraph a
completeDefinition opp vert = runST $ do
    connV <- do
        connV <- newArray_ (bounds opp) :: ST s (STArray s Int e)
        forM_ (assocs vert) $ \ (!vertex, !incident) ->
            forM_ (assocs incident) $ \ (!place, !dart) ->
                writeArray connV dart (vertex, place)
        freeze connV

    faces <- do
        visited <- newArray (bounds opp) False :: ST s (STArray s Int Bool)
        let walk start cur !l !path = do
                writeArray visited cur True
                let next =
                        let (v, p) = connV ! (opp ! cur)
                            s = vert ! v
                        in s ! (if p > 0 then p - 1 else snd $ bounds s)
                if next == start
                    then return $! listArray (0, l) $ reverse $ cur : path
                    else walk start next (l + 1) $ cur : path

        r <- foldM (\ !l !i -> do
                v <- readArray visited i
                if v then return l else walk i i 0 [] >>= (return $!) . (: l)
            ) [] (indices opp)

        return $! listArray (0, length r - 1) $ reverse r

    connF <- do
        connF <- newArray_ (bounds opp) :: ST s (STArray s Int e)
        forM_ (assocs faces) $ \ (!face, !incident) ->
            forM_ (assocs incident) $ \ (!place, !dart) ->
                writeArray connF dart (face, place)
        freeze connF

    return Graph
        { _opposite   = opp
        , _vertices   = vert
        , _connToVert = connV
        , _faces      = faces
        , _connToFace = connF
        }
