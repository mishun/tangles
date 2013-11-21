module Main (main) where

import qualified Data.Map as M
import System.IO (withFile, IOMode(..), hPrint)
import Control.Monad.State.Strict (execState, modify)
import Control.Monad (forM_, when)
import Text.Printf
import Diagrams.Prelude
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.IsomorphismTest
import Math.Topology.KnotTh.EmbeddedLink.TestPrime
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
import Math.Topology.KnotTh.Invariants
import TestUtil.Drawing


heuristic :: EmbeddedLink ArbitraryCrossing -> Bool
heuristic link =
    let test d a
            | beginVertex a == beginVertex d     = False
            | beginVertex a == beginVertex a'    = False
            | beginVertex d == beginVertex a'    = False
            | opposite (nextCCW a) /= nextCW a'  = False
            | passOver a == passOver a'          = True
            | opposite b == nextCW d             = passOver b == passOver (opposite b)
            | otherwise                          = test d b
            where
                a' = opposite a
                b = nextCCW a'
    in not $ or $ do
        d <- allHalfEdges link
        return $! test (opposite d) (nextCCW d)


generateVirtualKnots :: Int -> IO ()
generateVirtualKnots maxN = do
    let table = flip execState M.empty $
            tangleStarGlue
                AnyStar
                (\ yield -> forCCP_ (primeIrreducibleDiagrams maxN) $ \ (t, (s, _)) -> yield (t, s))
                (\ !link ->
                    when (eulerChar link == 0 && not (is1stOr2ndReidemeisterReducible link) && numberOfThreads link == 1 && testPrime link && heuristic link) $
                        modify $ M.insertWith' (++) (numberOfVertices link) [link]
                )

    forM_ (M.toList table) $ \ (n, links) ->
        writeSVGImage (printf "knot-diagrams-%i.svg" n) (Width 500) $ pad 1.05 $ flip execState mempty $
            forM_ links $ \ link ->
                modify (=== pad 1.1 (drawKnotDef link))

    forM_ (M.toList table) $ \ (n, links) ->
        withFile (printf "knot-diagram-codes-%i.txt" n) WriteMode $ \ f ->
            forM_ links $ \ link ->
                hPrint f $ encodeEdgeIndices link

    --forM_ (M.toList table) $ \ (n, links) ->
    --    forM_ links $ \ link -> do
    --        let jp = jonesPolynomial link
    --        putStrLn $ printf "%i: %s" n (show jp)

    print $ M.map length table


generateVirtualKnotProjections :: Int -> IO ()
generateVirtualKnotProjections maxN = do
    let table = flip execState M.empty $
            tangleStarGlue
                AnyStar
                (forCCP_ $ primeProjections maxN)
                (\ !link ->
                    when (eulerChar link == 0 && numberOfThreads link == 1 && testPrime link) $
                        modify (M.insertWith' (++) (numberOfVertices link) [link])
                )

    forM_ (M.toList table) $ \ (n, links) ->
        writeSVGImage (printf "knot-projections-%i.svg" n) (Width 500) $ pad 1.05 $ flip execState mempty $
            forM_ links $ \ link ->
                modify (=== pad 1.1 (drawKnotDef link))

    forM_ (M.toList table) $ \ (n, links) ->
        withFile (printf "knot-projection-codes-%i.txt" n) WriteMode $ \ f ->
            forM_ links $ \ link ->
                hPrint f $ encodeEdgeIndices link

    print $ M.map length table


generateAlternatingSkeletons :: Int -> IO ()
generateAlternatingSkeletons maxN = do
    let table = flip execState M.empty $
            tangleStarGlue
                BicolourableStar
                (forCCP_ $ templateProjections maxN)
                (\ !link ->
                    when (not (isReducable link) && testPrime link && not (has4LegPlanarPart link)) $
                        let g = (2 - eulerChar link) `div` 2
                            n = numberOfVertices link
                        in modify $ M.insertWith' (++) (n, g) [link]
                )

    forM_ [1 .. maxN] $ \ n -> do
        putStr $ show n ++ ":"
        forM_ [1 .. maxN] $ \ g ->
            putStr $ case M.lookup (n, g) table of
                Just x  -> printf "\t%i" $ length x
                Nothing -> "\t."
        putStrLn ""

    writeSVGImage "virtual.svg" (Width 500) $ pad 1.05 $
        flip execState mempty $
            forM_ (M.assocs table) $ \ ((_, g), lst) ->
                when (g == 1) $ forM_ lst $ \ link ->
                    modify (=== pad 1.1 (drawKnotDef link))


main :: IO ()
main = do
    let maxN = 2
    --generateAlternatingSkeletons maxN
    generateVirtualKnots maxN
    --generateVirtualKnotProjections maxN
