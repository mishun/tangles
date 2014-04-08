module Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
    ( torusDecomposition
    ) where

import Data.Function (fix)
import Data.List (sort, groupBy, partition)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Control.Monad.ST (runST)
import Control.Monad (foldM)
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

        tab = foldl (\ m (k, v) -> M.insertWith' (+) k v m) M.empty $ do
            let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
            PlanarChordDiagram a factor <- list
            let (trivial, nonTrivial) =
                    partition (all (== 0)) $
                        map (\ h -> max h $ map (* (-1)) h) $
                            homologyClasses a
                [x, y] = foldl (zipWith (+)) [0, 0] nonTrivial
            return ((x, y), factor * (circleFactor ^ length trivial))

        swap (a, b) = max (b, a) (-b, -a)

    in min (M.assocs tab) (sort $ map (\ (k, p) -> (swap k, p)) $ M.assocs tab)

