module Math.Manifolds.SurfaceGraph.Embedding
    ( embeddingInCircleWithVertexRooting
    , embeddingInCircleWithFaceRooting
    , embeddingInPolygonWithGrouping
    ) where

import Data.Maybe (fromJust)
import Data.List (find, groupBy)
import Data.Array.IArray ((!), array, listArray)
import Data.Array (Array)
import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.Barycentric
import Math.Manifolds.SurfaceGraph.Embedding.QuadraticInitialization
import Math.Manifolds.SurfaceGraph.Embedding.RelaxEmbedding


embeddingInCircleWithVertexRooting :: Int -> Vertex -> Array Dart [(Double, Double)]
embeddingInCircleWithVertexRooting smoothing vertex =
    circleEmbedding (max 1 smoothing) (Left vertex)


embeddingInCircleWithFaceRooting :: Int -> Face -> Array Dart [(Double, Double)]
embeddingInCircleWithFaceRooting smoothing face =
    circleEmbedding (max 1 smoothing) (Right face)


embeddingInPolygonWithGrouping :: (Dart -> Bool) -> Int -> Vertex -> (Int, Array Dart [(Double, Double)])
embeddingInPolygonWithGrouping sameGroupAsCW subdivisionOrder root
    | numberOfGroups < 3  = error "embeddingInPolygonWithGrouping: there are less than 3 groups"
    | otherwise           = (numberOfGroups, relaxEmbedding (Left root) embedding)
    where
        subdivision level v border
            | level <= 1  =
                let initial = quadraticInitialization 0.99 v border
                    g = vertexOwnerGraph v
                in listArray (dartsRange g) $ map (\ d -> [initial ! d, initial ! opposite d]) $ graphDarts g
            | otherwise   =
                let g = vertexOwnerGraph v
                    (_, vv, _, vd) = barycentricSubdivision' g
                in barycentricProjection g vd $
                    subdivision (level - 1) (fst $ fromJust $ find ((== v) . snd) vv) $ do
                        ((x, y), (x', y')) <- border `zip` (tail border ++ [ head border ])
                        [(x, y), (0.5 * (x + x'), 0.5 * (y + y'))]

        l = vertexDegree root

        groups = let ds = dartsIncidentToVertex root
                 in groupBy (const sameGroupAsCW) $
                     take (length ds) $
                         dropWhile sameGroupAsCW $
                             ds ++ ds

        groupLookup :: Array Int (Int, Int)
        groupLookup = array (0, l - 1) $ do
            (groupId, group) <- [0 ..] `zip` groups
            (inGroupId, d) <- [0 ..] `zip` group
            return (beginPlace d, (groupId, inGroupId))

        numberOfGroups = length groups

        groupSize :: Array Int Int
        groupSize = listArray (0, numberOfGroups - 1) $ map length groups

        embedding =
            let g = vertexOwnerGraph root
                (_, vv, _, vd) = barycentricSubdivision' g
            in barycentricProjection g vd $
                subdivision subdivisionOrder (fst $ fromJust $ find ((== root) . snd) vv) $ do
                    let put gi q p =
                            let halfAngle = -pi / fromIntegral numberOfGroups
                                a = 2 * halfAngle * fromIntegral gi
                                x0 = cos halfAngle
                                x = fromIntegral p / fromIntegral q
                                y0 = sin halfAngle * 2 * (x - 0.5);
                            in (cos a * x0 - sin a * y0, sin a * x0 + cos a * y0)

                    i <- [0 .. l - 1]
                    let (gi, p) = groupLookup ! i
                        sz = groupSize ! gi

                    [put gi (2 * sz) (2 * p + 1), put gi (2 * sz) (2 * p + 2)]


circleEmbedding :: Int -> Either Vertex Face -> Array Dart [(Double, Double)]
circleEmbedding subdivisionOrder root
    | subdivisionOrder <= 0  =
        case root of
            Left v ->
                let g = vertexOwnerGraph v
                    c = quadraticInitialization 0.99 v $
                        let k = vertexDegree v
                        in map (\ !i -> let a = 2 * pi * fromIntegral i / fromIntegral k in (cos a, -sin a)) [0 .. k - 1]
                in listArray (dartsRange g) $! map (\ d -> [c ! d, c ! opposite d]) $! graphDarts g
            _      -> error "internal error"

    | otherwise   =
        let g = case root of
                Left vertex -> vertexOwnerGraph vertex
                Right face  -> faceOwnerGraph face

            (_, vv, vf, vd) = barycentricSubdivision' g

            be = circleEmbedding (subdivisionOrder - 1) $ Left $
                case root of
                    Left vertex -> fst $ fromJust $ find ((== vertex) . snd) vv
                    Right face  -> fst $ fromJust $ find ((== face) . snd) vf

        in barycentricProjection g vd be


barycentricProjection :: SurfaceGraph -> [(Vertex, (Dart, Dart))] -> Array Dart [(Double, Double)] -> Array Dart [(Double, Double)]
barycentricProjection g vd be =
    array (dartsRange g) $ do
        (v, (a, b)) <- vd
        let l = reverse (be ! nthDartIncidentToVertex v 0)
                ++ tail (be ! nthDartIncidentToVertex v 2)
        [(a, l), (b, reverse l)]
