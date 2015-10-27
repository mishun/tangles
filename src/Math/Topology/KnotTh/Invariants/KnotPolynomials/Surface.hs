module Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
    ( torusMinimization
    ) where

import Control.Monad (guard)
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import Math.Topology.KnotTh.Algebra


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

        let (1, y2, x2) = extendedGCD x1 y1

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
