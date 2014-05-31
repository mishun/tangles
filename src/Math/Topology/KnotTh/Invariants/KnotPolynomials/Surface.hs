module Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
    ( torusDecomposition
    , kernelBasis
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.List (sortBy, partition, minimumBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Array as A
import Control.Monad.ST (runST)
import Control.Monad (foldM, guard)
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue


kernelBasis :: V.Vector (UV.Vector Int) -> V.Vector (UV.Vector Int)
kernelBasis =
    let gaussianElimination h w =
            let normalize v =
                    case UV.foldl1 gcd v of
                        0 -> v
                        g -> UV.map (`div` g) v

                go y x a | (y >= h) || (x >= w)  = a
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
                                    if i == y
                                        then row
                                        else let pivot' = row UV.! x
                                             in UV.zipWith (\ l r -> l * pivot - r * pivot') row lead
                                ) a
            in go 0 0

    in \ src ->
        case V.length src of
            0 -> V.empty
            m -> let n = UV.length (src V.! 0)
                     a = gaussianElimination n m $
                             V.generate n (\ y ->
                                 UV.generate m ((UV.! y) . (src V.!)) UV.++
                                     (UV.replicate n 0 UV.// [(y, 1)])
                             )
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


torusDecomposition :: (KauffmanXArg a) => EmbeddedLinkDiagram -> [((Int, Int), a)]
torusDecomposition link =
    let (tangle, star) = splitIntoTangleAndStar link
        l = numberOfLegs tangle

        (dim, homology) = cellularHomology $ vertexOwner star

        homologyClasses a = runST $ do
            visited <- UMV.replicate l False
            foldM (\ !list !start -> do
                    vs <- UMV.read visited start
                    if vs
                        then return list
                        else fmap (: list) $ fix (\ loop hom !i -> do
                                    c <- UMV.read visited i
                                    if c
                                        then return hom
                                        else do
                                            let d = nthOutcomingDart star i
                                                i' = endPlace d
                                                hom' = homology A.! d
                                            UMV.write visited i True
                                            UMV.write visited i' True
                                            loop (UV.zipWith (+) hom hom') (a UV.! i')
                                ) (UV.replicate dim 0) start
                ) [] [0 .. l - 1]

        tab = filter ((/= 0) . snd) $ M.assocs $
            foldl (\ m (k, v) -> M.insertWith' (+) k v m) M.empty $ do
                let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
                PlanarChordDiagram a factor <- list
                let (trivial, nonTrivial) =
                        partition (UV.all (== 0)) $
                            map (\ h -> max h $ UV.map (0 -) h) $
                                homologyClasses a
                    [x, y] = UV.toList $ foldl (UV.zipWith (+)) (UV.replicate dim 0) nonTrivial
                return ((x, y), factor * (circleFactor ^ length trivial))

    in min (canonicalForm tab)
           (canonicalForm $ map (\ ((x, y), value) -> ((x, -y), value)) tab)


canonicalForm :: (Ord a) => [((Int, Int), a)] -> [((Int, Int), a)]
canonicalForm list =
    let weight ((x, y), value) = (abs x + abs y, -x, -y, value)
    in minimumBy (comparing $ map weight) $ do
        (x1, y1) <- S.toList $ S.fromList $ do
            (x, y) <- case filter (/= (0, 0)) (map fst list) of
                          [] -> [(1, 0)]
                          l  -> l
            let g = gcd x y
                x' = x `div` g
                y' = y `div` g
            return $ max (x', y') (-x', -y')

        let (1, y2, x2) = extendedEuclid x1 y1

        n <- S.toList $ S.fromList $ (0 :) $ do
            ((x', y'), _) <- list
            let (x, y) | d >= 0     = (u, d)
                       | otherwise  = (-u, -d)
                    where d = x1 * y' - y1 * x'
                          u = x2 * y' + y2 * x'
            guard $ y /= 0

            let p = x `mod` y
                d = x `div` y
            case compare p (y - p) of
                EQ -> [d, d + 1]
                LT -> [d]
                GT -> [d + 1]

        return $ sortBy (comparing weight) $ do
            ((x, y), value) <- list
            let y' = -y1 * x + x1 * y
                x' = y2 * x + x2 * y - n * y'
            return (max (x', y') (-x', -y'), value)


extendedEuclid :: (Show a, Integral a) => a -> a -> (a, a, a)
extendedEuclid a 0 | a >= 0     = (a, 1, 0)
                   | otherwise  = (-a, -1, 0)
extendedEuclid a b =
    let (g, x, y) = extendedEuclid b (a `mod` b)
    in (g, y, x - (a `div` b) * y)
