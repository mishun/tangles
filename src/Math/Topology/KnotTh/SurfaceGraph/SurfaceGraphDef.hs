{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.SurfaceGraph.SurfaceGraphDef
    ( module Math.Topology.KnotTh.Algebra.PlanarAlgebra
    , SurfaceGraph
    , dual
    , constructFromList
    , barycentricSubdivision
    , barycentricSubdivision'
    , nthBarycentricSubdivision
    ) where

import Control.Monad (forM_, foldM)
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra.PlanarAlgebra


data SurfaceGraph a =
    Graph
        { _opposite   :: !(UV.Vector Int)
        , _vertices   :: !(V.Vector (UV.Vector Int))
        , _connToVert :: !(UV.Vector (Int, Int))
        , _faces      :: !(V.Vector (UV.Vector Int))
        , _connToFace :: !(UV.Vector (Int, Int))
        }


instance DartDiagram' SurfaceGraph where
    data Dart SurfaceGraph a = Dart !(SurfaceGraph a) {-# UNPACK #-} !Int

instance DartDiagram SurfaceGraph where
    dartOwner (Dart g _) = g
    dartIndex (Dart _ i) = i

    opposite (Dart g i) = Dart g (_opposite g `UV.unsafeIndex` i)

    nextBy n d = let (v, p) = beginPair d
                 in nthOutcomingDart v $ (p + n) `mod` vertexDegree v

    numberOfDarts g = UV.length (_opposite g)

    allDarts g = map (Dart g) [0 .. numberOfDarts g - 1]

    nthDart g i | i >= 0 && i < l  = Dart g i
                | otherwise        = error $ printf "nthDart: index %i is out of bounds [0, %i)" i l
        where l = UV.length (_opposite g)

    dartIndicesRange g = (0, numberOfDarts g - 1)

instance VertexDiagram' SurfaceGraph where
    data Vertex SurfaceGraph a = Vertex !(SurfaceGraph a) {-# UNPACK #-} !Int

instance VertexDiagram SurfaceGraph where
    vertexContent _ = undefined
    mapVertices = undefined

    vertexOwner (Vertex g _) = g
    vertexIndex (Vertex _ i) = i

    vertexDegree (Vertex g i) = UV.length (_vertices g V.! i)

    nthOutcomingDart v@(Vertex g i) j =
        let jj = j `mod` vertexDegree v
        in Dart g $ (_vertices g V.! i) UV.! jj

    numberOfVertices g = V.length (_vertices g)

    nthVertex g i | i >= 0 && i < l  = Vertex g i
                  | otherwise        = error $ printf "nthVertex: index %i is out of bounds [0, %i)" i l
        where
            l = V.length (_vertices g)

    allVertices g = map (Vertex g) [0 .. numberOfVertices g - 1]

    maybeBeginVertex d = Just $! beginVertex d

    beginPair (Dart g i) =
        let (vi, p) = _connToVert g UV.! i
        in (Vertex g vi, p)

    vertexIndicesRange g = (0, numberOfVertices g - 1)

instance SurfaceDiagram SurfaceGraph where
    numberOfFaces g = V.length (_faces g)

    nthFace g i | i >= 0 && i < l  = Face g i
                | otherwise        = error $ printf "nthFace: index %i is out of bounds [0, %i)" i l
        where
            l = V.length (_faces g)

    allFaces g = map (Face g) [0 .. numberOfFaces g - 1]

    data Face SurfaceGraph a = Face !(SurfaceGraph a) {-# UNPACK #-} !Int

    faceDegree (Face g i) = UV.length (_faces g V.! i)

    faceOwner (Face g _) = g

    faceIndex (Face _ i) = i

    leftPair (Dart g i) =
        let (fi, p) = _connToFace g UV.! i
        in (Face g fi, p)

    nthDartInCCWTraverse f@(Face g i) j =
        let jj = j `mod` faceDegree f
        in Dart g $ (_faces g V.! i) UV.! jj

    faceIndicesRange g = (0, numberOfFaces g - 1)

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
        s = UV.fromList $ map length g
        n = UV.length s

        offset = UV.fromList $ scanl (\ k i -> k + (s UV.! i)) 0 [0 .. n - 1]

        indexD (v, p)
            | v < 0 || v >= n           = error $ printf "constructFromList: vertex index %i is out of bound" v
            | p < 0 || p >= (s UV.! v)  = error $ printf "constructFromList: dart index %i is out of bound" p
            | otherwise                 = (offset UV.! v) + p

        opp = UV.fromList $ map indexD $ concat g

        idempotent =
            all (\ !i ->
                    let j = opp UV.! i
                    in (i /= j) && (i == opp UV.! j)
                ) [0 .. n - 1]

        vert = V.generate n $ \ i ->
                   let m = s UV.! i
                       o = offset UV.! i
                   in UV.generate m (+ o)


completeDefinition :: UV.Vector Int -> V.Vector (UV.Vector Int) -> SurfaceGraph a
completeDefinition opp vert =
    let connV = UV.create $ do
            cv <- UMV.new (UV.length opp)
            forM_ [0 .. V.length vert - 1] $ \ !vertex -> do
                let incident = vert V.! vertex
                forM_ [0 .. UV.length incident - 1] $ \ !place -> do
                    let dart = incident UV.! place
                    UMV.write cv dart (vertex, place) 
            return cv

        faces = ST.runST $ do
            visited <- UMV.replicate (UV.length opp) False
            let walk start cur !l !path = do
                    UMV.write visited cur True
                    let next =
                            let (v, p) = connV UV.! (opp UV.! cur)
                                s = vert V.! v
                            in s UV.! (if p > 0 then p - 1 else UV.length s - 1)
                    if next == start
                        then return $! UV.fromList $ reverse $ cur : path
                        else walk start next (l + 1) $ cur : path

            r <- foldM (\ !l !i -> do
                    v <- UMV.read visited i
                    if v then return l else walk i i (0 :: Int) [] >>= (return $!) . (: l)
                ) [] [0 .. UV.length opp - 1]

            return $! V.fromList $ reverse r

        connF = UV.create $ do
            cf <- UMV.new (UV.length opp)
            forM_ [0 .. V.length faces - 1] $ \ !face -> do
                let incident = faces V.! face 
                forM_ [0 .. UV.length incident - 1] $ \ !place -> do
                    let dart = incident UV.! place
                    UMV.write cf dart (face, place)
            return cf

    in Graph
        { _opposite   = opp
        , _vertices   = vert
        , _connToVert = connV
        , _faces      = faces
        , _connToFace = connF
        }


--      v1 (2)          e2 (4)                                 \ v2 (4)
--        *               |     f1 (3)                          *
--        |               |                                    /
-- f1 (3) | f0 (1)      --*---- e1 (2)                        / e1 (3)
--        |               |                     \            /
--        *               |     f0 (1)           \  e0 (1)  /
--      v0 (0)          e0 (0)             v0 (0) *--------* v1 (2)
barycentricSubdivision :: SurfaceGraph a -> SurfaceGraph b
barycentricSubdivision g = constructFromList $ concat [vertexPart, facePart, edgePart]
    where
        v = numberOfVertices g
        f = numberOfFaces g

        edges = allEdges g

        newFaceIndex fc = v + faceIndex fc

        edgeIndexLookup = A.array (0, numberOfDarts g - 1) $ do
            ((a, b), eId) <- edges `zip` [(v + f) ..]
            [(dartIndex a, eId), (dartIndex b, eId)]

        vertexPart = map make $ allVertices g
            where
                vertexToEdge d = (edgeIndexLookup A.! dartIndex d, if d < opposite d then 0 else 2)
                vertexToFace d = let (fc, place) = leftPair d
                                 in (newFaceIndex fc, 2 * place)
                make = concatMap (\ d -> [vertexToEdge d, vertexToFace d]) . outcomingDarts

        facePart = map make $ allFaces g
            where
                faceToEdge d = (edgeIndexLookup A.! dartIndex d, if d < opposite d then 3 else 1)
                faceToVertex d = let (ver, place) = beginPair d
                                 in (vertexIndex ver, 2 * place + 1)
                make = concatMap (\ d -> [faceToVertex d, faceToEdge d]) . faceTraverseCCW

        edgePart = map make edges
            where
                edgeToVertex d = let (ver, place) = beginPair d
                                 in (vertexIndex ver, 2 * place)
                edgeToFace d = let (fc, place) = leftPair d
                               in (newFaceIndex fc, 2 * place + 1)
                make (b, e) = [edgeToVertex b, edgeToFace e, edgeToVertex e, edgeToFace b]


barycentricSubdivision'
    :: SurfaceGraph a
        -> (SurfaceGraph b
           , [(Vertex SurfaceGraph b, Vertex SurfaceGraph a)]
           , [(Vertex SurfaceGraph b, Face SurfaceGraph a)]
           , [(Vertex SurfaceGraph b, (Dart SurfaceGraph a, Dart SurfaceGraph a))]
           )

barycentricSubdivision' g = (bs, zip (x [0 ..]) (allVertices g), zip (x [v ..]) (allFaces g), zip (x [v + f ..]) (allEdges g))
    where
        v = numberOfVertices g
        f = numberOfFaces g
        bs = barycentricSubdivision g
        x = map (nthVertex bs)


nthBarycentricSubdivision :: Int -> SurfaceGraph a -> SurfaceGraph a
nthBarycentricSubdivision n g | n > 0      = nthBarycentricSubdivision (n - 1) $ barycentricSubdivision g
                              | otherwise  = g
