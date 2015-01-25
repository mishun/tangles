module Main where

import Data.Ord (comparing)
import Data.Function (on)
import Data.List (sort, sortBy, groupBy, nub)
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Control.Monad (forM_, unless, guard)
import Text.Printf
import System.IO (withFile, IOMode(..), hPrint)
import System.Environment (getArgs)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, listChordDiagrams, genusOfChordDiagram)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TestPrime
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.EdgeIndicesEncoding
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Enumeration.DiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.Topology.KnotTh.Moves.MovesOfELink
import TestUtil.Table

{-
generateAlternatingSkeletons :: Int -> IO ()
generateAlternatingSkeletons maxN = do
    let table = flip execState M.empty $
            tangleStarGlue
                BicolourableStar
                (forCCP_ $ templateProjections maxN)
                (\ !link ->
                    when (not (isReidemeisterReducible link) && testPrime link && not (has4LegPlanarPart link)) $
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
-}

main :: IO ()
main = do
    [maxN] <- fmap (map read) getArgs

    let diagrams =
            filter (\ link -> not (isReidemeisterReducible link) && testPrime link) $
                tangleStarGlue
                    (filter ((== 1) . genusOfChordDiagram . fst) . listChordDiagrams . generateNonPlanarRaw)
                    (forCCP_ $ primeIrreducibleDiagrams maxN)

    do
        let classes = map (sortBy (comparing numberOfVertices) . allDiagrams) $
                equivalenceClasses (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink]) $
                    forM_ diagrams

        renderSVG "data/classes.svg" (Width 500) $ pad 1.05 $
            vcat' with { _sep = 0.6 } $ do
               cls <- classes
               guard $ any (\ l -> numberOfVertices l <= 4 && numberOfThreads l == 1) cls
               return $ hcat' with { _sep = 0.5 } $ do
                   link <- cls
                   return $ drawKnotDef link

        forM_ classes $ \ cls -> do
            let invs = sort $ map minimalKauffmanXPolynomial cls
            unless (all (== head invs) invs) $
                putStrLn $ "Class failed: " ++ show (nub invs)

    let sifted =
            siftByInvariant
                (\ l ->
                    ( minimalKauffmanXPolynomial l
                    -- , minimalKauffmanXPolynomial $ twistedDoubleSatelliteELink l
                    )
                ) $
                equivalenceClasses
                    (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink])
                    (forM_ diagrams)

    printTable "Embedded Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    do
        let classes = map (map representative) $ collisionClasses sifted

        renderSVG "data/collisions.svg" (Width 500) $ pad 1.05 $
            vcat' with { _sep = 0.5 } $ do
                cls <- classes
                return $ hcat' with { _sep = 0.2 } $ do
                    link <- cls
                    return $ drawKnotDef link

        forM_ (classes `zip` [1 :: Int ..]) $ \ (cls, i) -> do
            withFile (printf "data/collision_%i.txt" i) WriteMode $ \ handle ->
                mapM_ (hPrint handle . edgeIndicesEncoding) cls
            withFile (printf "data/collision_poly_%i" i) WriteMode $ \ handle ->
                mapM_ (hPrint handle . minimalKauffmanXPolynomial) cls

    do
        let group' f = groupBy (on (==) f) . sortBy (comparing f)

            paginate _ [] = []
            paginate n ls = let (h, t) = splitAt n ls
                            in h : paginate n t

            links = filter ((> 0) . numberOfVertices) $
                        mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted

        renderSVG "data/links.svg" (Width 500) $ pad 1.05 $
            vcat' with { _sep = 5 } $ do
                byThreads <- group' numberOfThreads links
                return $ hcat' with { _sep = 3 } $ do
                    byCross <- group' numberOfVertices byThreads
                    return $ hcat' with { _sep = 0.5 } $ do
                        page <- paginate 8 byCross
                        return $ vcat' with { _sep = 0.5 } $ do
                            link <- page
                            return $ drawKnotDef link

        forM_ (group' numberOfThreads links) $ \ byThreads ->
            forM_ (group' numberOfVertices byThreads) $ \ list -> do
                let n = numberOfVertices $ head list
                    k = numberOfThreads $ head list
                withFile (printf "data/links_%i^%i.txt" n k) WriteMode $ \ handle ->
                    mapM_ (hPrint handle . edgeIndicesEncoding) list
                withFile (printf "data/poly_%i^%i" n k) WriteMode $ \ handle ->
                    mapM_ (hPrint handle . minimalKauffmanXPolynomial) list
