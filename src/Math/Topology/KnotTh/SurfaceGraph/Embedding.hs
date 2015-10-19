module Math.Topology.KnotTh.SurfaceGraph.Embedding
    ( embeddingInCircleWithVertexRooting
    , embeddingInCircleWithFaceRooting
    , embeddingInPolygonWithGrouping
    ) where

import Data.Maybe (fromJust)
import Data.List (find, groupBy)
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as UV
import Math.Topology.KnotTh.SurfaceGraph.SurfaceGraphDef
import Math.Topology.KnotTh.SurfaceGraph.Embedding.QuadraticInitialization
import Math.Topology.KnotTh.SurfaceGraph.Embedding.RelaxEmbedding


embeddingInCircleWithVertexRooting :: Int -> Vertex SurfaceGraph a -> A.Array (Dart SurfaceGraph a) [(Double, Double)]
embeddingInCircleWithVertexRooting smoothing vertex =
    relaxEmbedding 0 (Left vertex) $
        circleEmbeddingInitialization smoothing (Left vertex)


embeddingInCircleWithFaceRooting :: Int -> Face SurfaceGraph a -> A.Array (Dart SurfaceGraph a) [(Double, Double)]
embeddingInCircleWithFaceRooting smoothing face =
    relaxEmbedding 0 (Right face) $
        circleEmbeddingInitialization (max 1 smoothing) (Right face)


embeddingInPolygonWithGrouping
    :: (Dart SurfaceGraph a -> Bool) -> Int -> Vertex SurfaceGraph a
        -> (Int, UV.Vector (Int, Int), A.Array (Dart SurfaceGraph a) [(Double, Double)])

embeddingInPolygonWithGrouping sameGroupAsCW subdivisionOrder root
    | numberOfGroups < 3  = error "embeddingInPolygonWithGrouping: there are less than 3 groups"
    | otherwise           = (numberOfGroups, groupLookup, relaxEmbedding numberOfGroups (Left root) embedding)
    where
        subdivision level v border
            | level <= 1  = quadraticEmbedding v border
            | otherwise   =
                let g = vertexOwner v
                    (_, vv, _, vd) = barycentricSubdivision' g
                in barycentricProjection g vd $
                    subdivision (level - 1) (fst $ fromJust $ find ((== v) . snd) vv) $ do
                        ((x, y), (x', y')) <- border `zip` (tail border ++ [ head border ])
                        [(x, y), (0.5 * (x + x'), 0.5 * (y + y'))]

        l = vertexDegree root

        groups = let ds = outcomingDarts root
                 in groupBy (const sameGroupAsCW) $
                     take (length ds) $
                         dropWhile sameGroupAsCW $
                             ds ++ ds

        groupLookup = (UV.replicate l (0, 0) UV.//) $ do
            (groupId, group) <- [0 ..] `zip` groups
            (inGroupId, d) <- [0 ..] `zip` group
            return (beginPlace d, (groupId, inGroupId))

        numberOfGroups = length groups


        groupSize = UV.fromListN numberOfGroups $ map length groups

        embedding =
            let g = vertexOwner root
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
                    let (gi, p) = groupLookup UV.! i
                        sz = groupSize UV.! gi
                    [put gi (2 * sz) (2 * p + 1), put gi (2 * sz) (2 * p + 2)]


circleEmbeddingInitialization
    :: Int -> Either (Vertex SurfaceGraph a) (Face SurfaceGraph a)
        -> A.Array (Dart SurfaceGraph a) [(Double, Double)]

circleEmbeddingInitialization subdivisionOrder root
    | subdivisionOrder <= 0  =
        case root of
            Left v ->
                quadraticEmbedding v $ do
                    let k = vertexDegree v
                    i <- [0 .. k - 1]
                    let a = 2 * pi * fromIntegral i / fromIntegral k
                    return (cos a, -sin a)
            _      -> error "internal error"

    | otherwise   =
        let g = case root of
                Left vertex -> vertexOwner vertex
                Right face  -> faceOwner face

            (_, vv, vf, vd) = barycentricSubdivision' g

        in barycentricProjection g vd $
            circleEmbeddingInitialization (subdivisionOrder - 1) $ Left $
                case root of
                    Left vertex -> fst $ fromJust $ find ((== vertex) . snd) vv
                    Right face  -> fst $ fromJust $ find ((== face) . snd) vf


barycentricProjection
    :: SurfaceGraph a -> [(Vertex SurfaceGraph b, (Dart SurfaceGraph a, Dart SurfaceGraph a))]
        -> A.Array (Dart SurfaceGraph b) [(Double, Double)]
            -> A.Array (Dart SurfaceGraph a) [(Double, Double)]

barycentricProjection g vd be =
    A.array (dartsRange g) $ do
        (v, (a, b)) <- vd
        let l = reverse (be A.! nthOutcomingDart v 0)
                ++ tail (be A.! nthOutcomingDart v 2)
        [(a, l), (b, reverse l)]


quadraticEmbedding :: Vertex SurfaceGraph a -> [(Double, Double)] -> A.Array (Dart SurfaceGraph a) [(Double, Double)]
quadraticEmbedding v border =
    let g = vertexOwner v
        c = quadraticInitialization 0.99 v border
    in A.listArray (dartsRange g) $ map (\ d -> [c A.! d, c A.! opposite d]) $ allDarts g
