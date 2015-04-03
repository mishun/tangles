module Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
    ( homologyDecomposition
    , torusMinimization
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.List (sortBy, minimumBy)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Array as A
import Control.Monad.ST (runST)
import Control.Monad (foldM, guard)
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.Manifolds.SurfaceGraph.Homology
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.EmbeddedLink


homologyDecomposition :: (KauffmanXArg a) => EmbeddedLinkDiagram -> (Int, [([UV.Vector Int], a)])
homologyDecomposition link =
    let (tangle, star) = splitIntoTangleAndStar link
        l = numberOfLegs tangle
        (dim, homology) = cellularHomology $ vertexOwner star

        homologyClasses a = runST $ do
            visited <- UMV.replicate l False
            foldM (\ !list !start -> do
                    vs <- UMV.read visited start
                    if vs
                        then return list
                        else do
                            lp <- fix (\ loop hom !i -> do
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
                            return $ max lp (UV.map (0 -) lp) : list
                ) [] [0 .. l - 1]

        tokens = do
            PlanarChordDiagram a factor <-
                let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
                in list
            return (homologyClasses a, factor)

    in (dim, tokens)


torusMinimization :: (Ord a) => [((Int, Int), a)] -> [((Int, Int), a)]
torusMinimization list =
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
