module Math.Topology.Manifolds.SurfaceGraph.Barycentric
    ( barycentricSubdivision
    , barycentricSubdivision'
    , nthBarycentricSubdivision
    ) where

import Data.Array (array, (!))
import Math.Topology.Manifolds.SurfaceGraph.Definition


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

        edgeIndexLookup = array (0, numberOfDarts g - 1) $
            concatMap (\ ((a, b), eId) -> [(dartIndex a, eId), (dartIndex b, eId)]) $
                zip edges [(v + f) ..]

        vertexPart = map make $ allVertices g
            where
                vertexToEdge d = (edgeIndexLookup ! dartIndex d, if d < opposite d then 0 else 2)
                vertexToFace d = let (fc, place) = leftPair d
                                 in (newFaceIndex fc, 2 * place)
                make = concatMap (\ d -> [vertexToEdge d, vertexToFace d]) . outcomingDarts

        facePart = map make $ allFaces g
            where
                faceToEdge d = (edgeIndexLookup ! dartIndex d, if d < opposite d then 3 else 1)
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
nthBarycentricSubdivision n g
    | n > 0      = nthBarycentricSubdivision (n - 1) $ barycentricSubdivision g
    | otherwise  = g
