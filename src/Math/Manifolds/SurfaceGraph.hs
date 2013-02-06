module Math.Manifolds.SurfaceGraph
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
    , Cell(..)

    , nextCCW
    , nextCW
    , opposite
    , begin
    , left
    , numberOfVertices
    , graphVertices
    , verticesRange
    , nthVertex
    , vertexDegree
    , dartsIncidentToVertex
    , nthDartIncidentToVertex
    , numberOfFaces
    , graphFaces
    , facesRange
    , faceDegree
    , faceTraverseCCW
    , numberOfDarts
    , numberOfEdges
    , graphDarts
    , dartsRange
    , dual

    , constructFromList
    , constructFromArray

    , graphEdges
    , end
    , right
    , faceTraverseCW
    , toPair
    , adjacentVertices
    , beginVertex
    , endVertex
    , eulerChar
    , isTriangulation
    ) where

import Data.List (intercalate)
import Data.Ord
import Data.Ix
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST (STArray)
import Control.Monad.ST (ST, runST)
import Control.Monad
import Text.Printf


data SurfaceGraph = Graph
    { _opposite   :: {-# UNPACK #-} !(UArray Int Int)
    , _vertices   :: {-# UNPACK #-} !(Array Int (UArray Int Int))
    , _connToVert :: {-# UNPACK #-} !(Array Int (Int, Int))
    , _faces      :: {-# UNPACK #-} !(Array Int (UArray Int Int))
    , _connToFace :: {-# UNPACK #-} !(Array Int (Int, Int))
    }

instance Show SurfaceGraph where
    show =
        let showVertex v =
                let inc = intercalate ", " $ map (show . toPair . opposite) $ dartsIncidentToVertex v
                in concat [show $ vertexIndex v, " -> {", inc, "}"]
        in (\ s -> concat ["{\n", s, "\n}"]) . unlines . map showVertex . graphVertices


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


data Cell = Cell0D Vertex | Cell1D Dart | Cell2D Face deriving (Eq, Ord)


nextCCW :: Dart -> Dart
nextCCW (Dart g i) = Dart g $! (v !) $! if p + 1 == k then 0 else p + 1
    where
        (vi, p) = _connToVert g ! i
        v = _vertices g ! vi
        k = (+ 1) $! snd $! bounds v


nextCW :: Dart -> Dart
nextCW (Dart g i) = Dart g $! (v !) $! if p == 0 then k - 1 else p - 1
    where
        (vi, p) = _connToVert g ! i
        v = _vertices g ! vi
        k = (+ 1) $! snd $! bounds v


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


numberOfVertices :: SurfaceGraph -> Int
numberOfVertices g = (+ 1) $! snd $! bounds $! _vertices g


graphVertices :: SurfaceGraph -> [Vertex]
graphVertices g = map (Vertex g) [0 .. snd $! bounds $! _vertices g]


verticesRange :: SurfaceGraph -> (Vertex, Vertex)
verticesRange g = (Vertex g 0, Vertex g $! numberOfVertices g - 1)


nthVertex :: SurfaceGraph -> Int -> Vertex
nthVertex g i
    | inRange (bounds $! _vertices g) i  = Vertex g i
    | otherwise                          = error $ printf "nthVertex: index %i is out of bound" i


vertexDegree :: Vertex -> Int
vertexDegree (Vertex g i) = (+ 1) $! snd $! bounds (_vertices g ! i)


dartsIncidentToVertex :: Vertex -> [Dart]
dartsIncidentToVertex (Vertex g i) = map (Dart g) $! elems (_vertices g ! i)


nthDartIncidentToVertex :: Vertex -> Int -> Dart
nthDartIncidentToVertex v@(Vertex g i) j =
    let k = vertexDegree v
        jj = (k + (j `rem` k)) `rem` k
    in Dart g $! (_vertices g ! i) ! jj


numberOfFaces :: SurfaceGraph -> Int
numberOfFaces g = (+ 1) $! snd $! bounds $! _faces g


facesRange :: SurfaceGraph -> (Face, Face)
facesRange g = (Face g 0, Face g $! numberOfFaces g - 1)


faceDegree :: Face -> Int
faceDegree (Face g i) = (+ 1) $! snd $! bounds (_faces g ! i)


faceTraverseCCW :: Face -> [Dart]
faceTraverseCCW (Face g i) = map (Dart g) $! elems (_faces g ! i)


graphFaces :: SurfaceGraph -> [Face]
graphFaces g = map (Face g) [0 .. snd $! bounds $! _faces g]


numberOfDarts :: SurfaceGraph -> Int
numberOfDarts g = (+ 1) $! snd $! bounds $! _opposite g


numberOfEdges :: SurfaceGraph -> Int
numberOfEdges = (`div` 2) . numberOfDarts


graphDarts :: SurfaceGraph -> [Dart]
graphDarts g = map (Dart g) [0 .. snd $! bounds $! _opposite g]


dartsRange :: SurfaceGraph -> (Dart, Dart)
dartsRange g =
    let (l, r) = bounds $! _opposite g
    in (Dart g l, Dart g r)


dual :: SurfaceGraph -> SurfaceGraph
dual g = Graph
    { _opposite   = _opposite g
    , _vertices   = _faces g
    , _connToVert = _connToFace g
    , _faces      = _vertices g
    , _connToFace = _connToVert g
    }



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

    return $! Graph
        { _opposite   = opp
        , _vertices   = vert
        , _connToVert = connV
        , _faces      = faces
        , _connToFace = connF
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

        idempotent = all (\ (i, j) -> i == opp ! j) $! assocs opp

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



graphEdges :: SurfaceGraph -> [(Dart, Dart)]
graphEdges = filter (\ (a, b) -> dartIndex a < dartIndex b) . map (\ d -> (d, opposite d)) . graphDarts


end :: Dart -> (Vertex, Int)
end = begin . opposite


right :: Dart -> (Face, Int)
right = left . opposite


faceTraverseCW :: Face -> [Dart]
faceTraverseCW = reverse . map opposite . faceTraverseCCW


toPair :: Dart -> (Int, Int)
toPair d = let (v, p) = begin d in (vertexIndex v, p)


adjacentVertices :: Vertex -> [Vertex]
adjacentVertices = map (fst . begin . opposite) . dartsIncidentToVertex


beginVertex :: Dart -> Vertex
beginVertex = fst . begin


endVertex :: Dart -> Vertex
endVertex = fst . begin . opposite


eulerChar :: SurfaceGraph -> Int
eulerChar g = v + f - e
    where
        v = numberOfVertices g
        f = numberOfFaces g
        e = numberOfEdges g


isTriangulation :: SurfaceGraph -> Bool
isTriangulation = all ((== 3) . faceDegree) . graphFaces
