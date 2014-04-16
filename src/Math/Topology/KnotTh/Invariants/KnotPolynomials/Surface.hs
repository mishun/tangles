module Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
    ( torusDecomposition
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy, partition, minimumBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Control.Monad.ST (runST)
import Control.Monad (foldM, guard)
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue


torusDecomposition :: (KauffmanXArg a) => EmbeddedLinkDiagram -> [((Int, Int), a)]
torusDecomposition link =
    let (tangle, star) = splitIntoTangleAndStar link
        l = numberOfLegs tangle

        border = (V.replicate l undefined V.//) $ do
            (pair, gr) <-
                let ds =
                        let isBigon d = nextCCW (opposite d) == opposite (nextCW d)
                        in groupBy (const isBigon) $
                            let (pre, post) = span isBigon (outcomingDarts star)
                            in post ++ pre
                in case length ds of
                    4 -> [[1, 0], [0, 1], [-1, 0], [0, -1]] `zip` ds
                    6 -> [[1, 0], [1, 1], [0, 1], [-1, 0], [-1, -1], [0, -1]] `zip` ds
                    _ -> error "internal error"

            d <- gr
            return (beginPlace d, (endPlace d, pair :: [Int]))

        homologyClasses a = runST $ do
            visited <- UMV.replicate l False
            foldM (\ !list !start -> do
                    vs <- UMV.read visited start
                    if vs
                        then return list
                        else do
                            homology <- fix (\ loop hom !i -> do
                                    c <- UMV.read visited i
                                    if c
                                        then return hom
                                        else do
                                            let (i', hom') = border V.! i
                                            UMV.write visited i True
                                            UMV.write visited i' True
                                            loop (zipWith (+) hom hom') (a UV.! i')
                                ) (replicate (2 * genus link) 0) start
                            return $ homology : list
                ) [] [0 .. l - 1]

        tab = filter ((/= 0) . snd) $ M.assocs $
            foldl (\ m (k, v) -> M.insertWith' (+) k v m) M.empty $ do
                let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
                PlanarChordDiagram a factor <- list
                let (trivial, nonTrivial) =
                        partition (all (== 0)) $
                            map (\ h -> max h $ map (* (-1)) h) $
                                homologyClasses a
                    [x, y] = foldl (zipWith (+)) [0, 0] nonTrivial
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
