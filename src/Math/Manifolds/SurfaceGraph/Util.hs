module Math.Manifolds.SurfaceGraph.Util
    ( graphVertices
    , verticesRange
    , dartsIncidentToVertex
    , graphFaces
    , facesRange
    , faceTraverseCCW
    , faceTraverseCW
    , numberOfEdges
    , graphDarts
    , dartsRange
    , graphEdges
    , nextCCW
    , nextCW
    , end
    , right
    , beginVertex
    , endVertex
    , beginPlace
    , endPlace
    , leftFace
    , rightFace
    , adjacentVertices
    , eulerChar
    , isTriangulation
    ) where

import Math.Manifolds.SurfaceGraph.Definition


graphVertices :: SurfaceGraph -> [Vertex]
graphVertices g = map (nthVertex g) [0 .. numberOfVertices g - 1]


verticesRange :: SurfaceGraph -> (Vertex, Vertex)
verticesRange g = (nthVertex g 0, nthVertex g $ numberOfVertices g - 1)


dartsIncidentToVertex :: Vertex -> [Dart]
dartsIncidentToVertex v = map (nthDartIncidentToVertex v) [0 .. vertexDegree v - 1]


graphFaces :: SurfaceGraph -> [Face]
graphFaces g = map (nthFace g) [0 .. numberOfFaces g - 1]


facesRange :: SurfaceGraph -> (Face, Face)
facesRange g = (nthFace g 0, nthFace g $ numberOfFaces g - 1)


faceTraverseCCW :: Face -> [Dart]
faceTraverseCCW f = map (nthDartInFaceTraverseCCW f) [0 .. faceDegree f - 1]


faceTraverseCW :: Face -> [Dart]
faceTraverseCW = reverse . map opposite . faceTraverseCCW


numberOfEdges :: SurfaceGraph -> Int
numberOfEdges = (`div` 2) . numberOfDarts


graphDarts :: SurfaceGraph -> [Dart]
graphDarts g = map (nthDart g) [0 .. numberOfDarts g - 1]


dartsRange :: SurfaceGraph -> (Dart, Dart)
dartsRange g = (nthDart g 0, nthDart g $ numberOfDarts g - 1)


graphEdges :: SurfaceGraph -> [(Dart, Dart)]
graphEdges = filter (\ (a, b) -> dartIndex a < dartIndex b) . map (\ d -> (d, opposite d)) . graphDarts


nextCCW, nextCW :: Dart -> Dart
nextCCW = nextBy 1
nextCW = nextBy (-1)


end :: Dart -> (Vertex, Int)
end = begin . opposite


right :: Dart -> (Face, Int)
right = left . opposite


beginVertex, endVertex :: Dart -> Vertex
beginVertex = fst . begin
endVertex = fst . end


beginPlace, endPlace :: Dart -> Int
beginPlace = snd . begin
endPlace = snd . end


leftFace, rightFace :: Dart -> Face
leftFace = fst . left
rightFace = fst . right


adjacentVertices :: Vertex -> [Vertex]
adjacentVertices = map (fst . begin . opposite) . dartsIncidentToVertex


eulerChar :: SurfaceGraph -> Int
eulerChar g = v + f - e
    where
        v = numberOfVertices g
        f = numberOfFaces g
        e = numberOfEdges g


isTriangulation :: SurfaceGraph -> Bool
isTriangulation = all ((== 3) . faceDegree) . graphFaces
