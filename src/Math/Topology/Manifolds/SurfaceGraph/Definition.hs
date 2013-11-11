module Math.Topology.Manifolds.SurfaceGraph.Definition
    ( SurfaceGraph
    , Dart
    , dartIndex
    , dartOwnerGraph
    , Vertex
    , vertexIndex
    , vertexOwnerGraph
    , Face
    , faceIndex
    , faceOwnerGraph
    , numberOfVertices
    , nthVertex
    , vertexDegree
    , nthDartIncidentToVertex
    , numberOfFaces
    , nthFace
    , faceDegree
    , nthDartInFaceTraverseCCW
    , numberOfDarts
    , nthDart
    , nextBy
    , opposite
    , begin
    , left
    , toPair
    , dual
    , constructFromList
    , constructFromArray
    ) where

import Data.Ix (Ix(..))
import Data.Ord (comparing)
import Data.List (intercalate)
import Data.Array.IArray (IArray, (!), bounds, listArray, elems, assocs, indices)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, foldM)
import Text.Printf


data SurfaceGraph = Graph
    { _opposite   :: {-# UNPACK #-} !(UArray Int Int)
    , _vertices   :: {-# UNPACK #-} !(Array Int (UArray Int Int))
    , _connToVert :: {-# UNPACK #-} !(Array Int (Int, Int))
    , _faces      :: {-# UNPACK #-} !(Array Int (UArray Int Int))
    , _connToFace :: {-# UNPACK #-} !(Array Int (Int, Int))
    }


data Dart = Dart { dartOwnerGraph :: !SurfaceGraph, dartIndex :: {-# UNPACK #-} !Int }

instance Eq Dart where
    (==) a b = dartIndex a == dartIndex b

instance Ord Dart where
    compare = comparing dartIndex

instance Ix Dart where
    range (Dart _ a, Dart g b) = map (Dart g) [a .. b]
    index (Dart _ a, Dart _ b) (Dart _ c) = index (a, b) c
    inRange (Dart _ a, Dart _ b) (Dart _ c) = c >= a && c <= b
    rangeSize (Dart _ a, Dart _ b) = max 0 $ b - a + 1


data Vertex = Vertex { vertexOwnerGraph :: !SurfaceGraph, vertexIndex :: {-# UNPACK #-} !Int }

instance Eq Vertex where
    (==) a b = vertexIndex a == vertexIndex b

instance Ord Vertex where
    compare = comparing vertexIndex

instance Ix Vertex where
    range (Vertex _ a, Vertex g b) = map (Vertex g) [a .. b]
    index (Vertex _ a, Vertex _ b) (Vertex _ c) = index (a, b) c
    inRange (Vertex _ a, Vertex _ b) (Vertex _ c) = c >= a && c <= b
    rangeSize (Vertex _ a, Vertex _ b) = max 0 $ b - a + 1


data Face = Face { faceOwnerGraph :: !SurfaceGraph, faceIndex :: {-# UNPACK #-} !Int }

instance Eq Face where
    (==) a b = faceIndex a == faceIndex b

instance Ord Face where
    compare = comparing faceIndex

instance Ix Face where
    range (Face _ a, Face g b) = map (Face g) [a .. b]
    index (Face _ a, Face _ b) (Face _ c) = index (a, b) c
    inRange (Face _ a, Face _ b) (Face _ c) = c >= a && c <= b
    rangeSize (Face _ a, Face _ b) = max 0 $ b - a + 1


numberOfVertices :: SurfaceGraph -> Int
numberOfVertices g = (+ 1) $ snd $ bounds $ _vertices g


nthVertex :: SurfaceGraph -> Int -> Vertex
nthVertex g i | inRange (bounds $ _vertices g) i  = Vertex g i
              | otherwise                         = error $ printf "nthVertex: index %i is out of bounds" i


vertexDegree :: Vertex -> Int
vertexDegree (Vertex g i) = (+ 1) $ snd $ bounds (_vertices g ! i)


nthDartIncidentToVertex :: Vertex -> Int -> Dart
nthDartIncidentToVertex v@(Vertex g i) j =
    let jj = j `mod` vertexDegree v
    in Dart g $! (_vertices g ! i) ! jj


numberOfFaces :: SurfaceGraph -> Int
numberOfFaces g = (+ 1) $! snd $! bounds $! _faces g


nthFace :: SurfaceGraph -> Int -> Face
nthFace g i | inRange (bounds $ _faces g) i  = Face g i
            | otherwise                      = error $ printf "nthFace: index %i is out of bounds" i


faceDegree :: Face -> Int
faceDegree (Face g i) = (+ 1) $! snd $! bounds (_faces g ! i)


nthDartInFaceTraverseCCW :: Face -> Int -> Dart
nthDartInFaceTraverseCCW f@(Face g i) j =
    let jj = j `mod` faceDegree f
    in Dart g $! (_faces g ! i) ! jj


numberOfDarts :: SurfaceGraph -> Int
numberOfDarts g = (+ 1) $! snd $! bounds $! _opposite g


nthDart :: SurfaceGraph -> Int -> Dart
nthDart g i | inRange (bounds $ _opposite g) i  = Dart g i
            | otherwise                         = error $ printf "nthDart: index %i is out of bounds" i


nextBy :: Int -> Dart -> Dart
nextBy x (Dart g i) = Dart g $ (v !) $ (p + x) `mod` k
    where
        (vi, p) = _connToVert g ! i
        v = _vertices g ! vi
        k = snd (bounds v) + 1


opposite :: Dart -> Dart
opposite (Dart g i) = Dart g (_opposite g ! i)


begin :: Dart -> (Vertex, Int)
begin (Dart g i) =
    let (vi, p) = _connToVert g ! i
    in (Vertex g vi, p)


left :: Dart -> (Face, Int)
left (Dart g i) =
    let (fi, p) = _connToFace g ! i
    in (Face g fi, p)


toPair :: Dart -> (Int, Int)
toPair d =
    let (v, p) = begin d
    in (vertexIndex v, p)


instance Show SurfaceGraph where
    show graph =
        let showVertex v =
                let inc = intercalate ", " $ map (show . toPair . opposite . nthDartIncidentToVertex v) [0 .. vertexDegree v - 1]
                in concat [show $ vertexIndex v, " -> {", inc, "}"]
        in (\ s -> concat ["{\n", s, "\n}"]) $ unlines $ map (showVertex . nthVertex graph) [0 .. numberOfVertices graph - 1]


dual :: SurfaceGraph -> SurfaceGraph
dual g = Graph
    { _opposite   = _opposite g
    , _vertices   = _faces g
    , _connToVert = _connToFace g
    , _faces      = _vertices g
    , _connToFace = _connToVert g
    }


constructFromList :: [[(Int, Int)]] -> SurfaceGraph
constructFromList g
    | not idempotent  = error "constructFromList: bad connections"
    | otherwise       = completeDefinition opp vert
    where
        n = length g
        s = listArray (0, n - 1) $! map length g :: UArray Int Int
        offset = listArray (0, n) $! scanl (\ k i -> k + s ! i) 0 [0 .. n - 1] :: UArray Int Int

        indexD (v, p)
            | v < 0 || v >= n        = error $ printf "constructFromList: vertex index %i is out of bound" v
            | p < 0 || p >= (s ! v)  = error $ printf "constructFromList: dart index %i is out of bound" p
            | otherwise              = (offset ! v) + p

        opp = listArray (0, (offset ! n) - 1) $! map indexD $! concat g

        idempotent = all (\ (i, j) -> (i /= j) && (i == opp ! j)) $! assocs opp

        vert = listArray (0, n - 1) $! map (\ i ->
                let m = s ! i
                    o = offset ! i
                in listArray (0, m - 1) $! map (+ o) [0 .. m - 1]
            ) [0 .. n - 1]


constructFromArray :: (Ix i, Ix j, IArray extA (intA j (i, j)), IArray intA (i, j)) => extA i (intA j (i, j)) -> SurfaceGraph
constructFromArray g =
    constructFromList $! map (map (\ (i, j) ->
        let is | inRange (bounds g) i  = index (bounds g) i
               | otherwise             = error "constructFromArray: vertex index is out of bound"
            ja = g ! i
            js | inRange (bounds ja) j  = index (bounds ja) j
               | otherwise              = error "constructFromArray: dart index is out of bound"
        in (is, js)
    ) . elems) $! elems g


completeDefinition :: UArray Int Int -> Array Int (UArray Int Int) -> SurfaceGraph
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
                        in s ! (if p > 0 then p - 1 else snd $! bounds s)
                if next == start
                    then return $! listArray (0, l) $! reverse $! cur : path
                    else walk start next (l + 1) $! cur : path

        r <- foldM (\ !l !i -> do
                v <- readArray visited i
                if v then return l else walk i i 0 [] >>= (return $!) . (: l)
            ) [] (indices opp)

        return $! listArray (0, length r - 1) $! reverse r

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
