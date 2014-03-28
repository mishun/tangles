module Main (main) where

import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List (sortBy, groupBy)
import System.IO (withFile, IOMode(..), hPrint)
import Control.Monad.State.Strict (execState, modify)
import Control.Monad.Writer (execWriter, tell)
import Control.Monad (forM_, when)
import Text.Printf
import Diagrams.Prelude
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TestPrime
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
import Math.Topology.KnotTh.EdgeIndicesEncoding
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Enumeration.DiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import TestUtil.Table
import TestUtil.Drawing


generateVirtualKnots :: Int -> IO ()
generateVirtualKnots maxN = do
    let is1stOr2ndReidemeisterReducible knot = or $ do
            c <- allVertices knot
            a <- outcomingDarts c
            let b = opposite a
                r1 = nextCCW a == b
                r2 = isDart b && (passOver a == passOver b) && (nextCCW a == opposite (nextCW b))
            return $! r1 || r2

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

    let diagrams = flip execState [] $
            tangleStarGlue
                AnyStar
                (forCCP_ $ primeIrreducibleDiagrams maxN)
                (\ !link ->
                    when (eulerChar link == 0 && not (is1stOr2ndReidemeisterReducible link) && numberOfThreads link == 1 && testPrime link && heuristic link) $
                        modify (link :)
                )

    let results =
            groupBy (on (==) (numberOfVertices . head . snd)) $ sortBy (comparing $ numberOfVertices . head . snd) $
                map (\ pairs@((poly, _) : _) -> (poly, sortBy (comparing numberOfVertices) $ map snd pairs)) $
                    filter (not . null) $ groupBy (on (==) fst) $ sortBy (comparing fst) $
                        map (\ d -> (minimalKauffmanXPolynomial d, d)) diagrams

    mapM_ (print . fst) $ concat results

    writeSVGImage "diagrams.svg" (Width 500) $ pad 1.05 $
        flip execState mempty $
            forM_ results $ \ ofSize ->
                let pack = flip execState mempty $
                        forM_ ([1 :: Int ..] `zip` ofSize) $ \ (i, (poly, reps)) ->
                            let line = flip execState mempty $ do
                                    forM_ reps $ \ link -> modify (||| pad 1.1 (drawKnotDef link # freeze # scale 8))
                                    modify $ (|||) $ text (show poly) <> strutX 50
                                    modify $ (|||) $ text (show i ++ ".") <> strutX 3
                            in modify (=== line)
                in modify (=== strutY 5) >> modify (=== pack)

    print $ map length results


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
                hPrint f $ edgeIndicesEncoding link

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
    let maxN = 5
    --generateAlternatingSkeletons maxN
    --generateVirtualKnots maxN
    --generateVirtualKnotProjections maxN
    let diagrams = flip execState [] $
            tangleStarGlue
                AnyStar
                (forCCP_ $ primeIrreducibleDiagrams maxN)
                (\ !link ->
                    when (eulerChar link == 0 && not (isReidemeisterReducible link) && testPrime link && numberOfThreads link == 1) $
                        modify (link :)
                )

    let sifted =
            siftByInvariant minimalKauffmanXPolynomial $
                equivalenceClasses
                    --[map AdHoc.greedyReidemeisterReduction . searchMoves [flype, pass1, pass2, pass3]]
                    [map reidemeisterReduction . goReidemeisterIII]
                    (forM_ diagrams)

    printTable "Embedded Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)
    writeSVGImage "collisions.svg" (Width 500) $ pad 1.05 $ execWriter $
        forM_ (collisionClasses sifted `zip` [0 ..]) $ \ (cc, j) ->
            forM_ (cc `zip` [0 ..]) $ \ (info, i) ->
                tell $ translate (r2 (2.2 * i, -2.2 * j)) $ drawKnotDef $ representative info
