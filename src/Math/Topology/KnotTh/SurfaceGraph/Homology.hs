module Math.Topology.KnotTh.SurfaceGraph.Homology
    ( module Math.Topology.KnotTh.SurfaceGraph
    , cellularHomology
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Array as A
import Math.Topology.KnotTh.SurfaceGraph


kernelBasis :: V.Vector (UV.Vector Int) -> V.Vector (UV.Vector Int)
kernelBasis =
    let gaussianElimination h w =
            let normalize v =
                    let g = UV.foldl1 gcd v
                    in if g <= 1
                        then v
                        else UV.map (`div` g) v

                go y x a | (y >= h) || (x >= w)  = V.map normalize a
                         | otherwise             =
                    let lead = a V.! y
                    in case lead UV.! x of
                        0     ->
                            case V.findIndex ((/= 0) . (UV.! x)) $ V.drop (y + 1) a of
                                Nothing -> go y (x + 1) a
                                Just dy -> go y x $ V.modify (\ a' -> MV.swap a' y (y + 1 + dy)) a

                        pivot -> go (y + 1) (x + 1) $
                            V.map normalize $ V.imap
                                (\ i row ->
                                    case i == y of
                                        True | pivot > 0 -> row
                                             | otherwise -> UV.map (0 -) row
                                        False            ->
                                            let pivot' = row UV.! x
                                            in UV.zipWith (\ l r -> l * pivot - r * pivot') row lead
                                ) a
            in go 0 0

    in \ src ->
        case V.length src of
            0 -> V.empty
            m -> let n = UV.length (src V.! 0)
                     a = gaussianElimination n m $
                             V.generate n $ \ y ->
                                 UV.generate m ((UV.! y) . (src V.!)) UV.++
                                     (UV.replicate n 0 UV.// [(y, 1)])
                 in V.map snd $ V.filter (UV.all (== 0) . fst) $ V.map (UV.splitAt m) a


cellularHomology :: SurfaceGraph a -> (Int, A.Array (Dart SurfaceGraph a) (UV.Vector Int))
cellularHomology graph =
    let vars = A.array (dartsRange graph) $ do
            (i, (a, b)) <- [0 ..] `zip` allEdges graph
            [(a, (i, 1)), (b, (i, -1))]

        basis = kernelBasis $
            V.fromList $ do
                f <- allFaces graph
                return $ UV.accum (+) (UV.replicate (numberOfEdges graph) 0) $
                    map (vars A.!) (faceTraverseCCW f)

        dim = V.length basis

        homology = A.array (dartsRange graph) $ do
            (i, (a, b)) <- [0 ..] `zip` allEdges graph
            let h = UV.generate dim $ \ j -> (basis V.! j) UV.! i
            [(a, h), (b, UV.map (0 -) h)]

    in (dim, homology)
