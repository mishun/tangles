module Main where

import Control.Arrow ((&&&))
import Control.Monad (forM_, unless, when)
import Data.Function (on)
import Data.List (sort, sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hPrint, hPutStrLn)
import Text.Printf
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Math.Topology.KnotTh.ChordDiagram (generateNonPlanarRaw, listChordDiagrams, genusOfChordDiagram)
import Math.Topology.KnotTh.Knotted.EdgeIndicesEncoding
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Tabulation.TangleStarGlue
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Enumeration.DiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.Topology.KnotTh.Moves.MovesOfELink
import Math.Topology.KnotTh.Tangle
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
            let cds = map (\ n ->filter ((== 1) . genusOfChordDiagram . fst) $ listChordDiagrams $ generateNonPlanarRaw n) [0 ..]
            in filter (\ link -> not (isReidemeisterReducible link) && testPrime link) $
                tangleStarGlue (cds !!) (forCCP_ $ primeIrreducibleDiagrams maxN)

    let invariantSet link = ( minimalKauffmanXPolynomial link
                            -- , minimalKauffmanXPolynomial $ twistedSatellite 2 link
                            )

    do
        let classes = map (sortBy (comparing numberOfVertices) . allDiagrams) $
                equivalenceClasses (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink]) $
                    forM_ diagrams
{-
        renderSVG "vlinks/classes.svg" (Width 500) $ pad 1.05 $
            vsep 0.6 $ do
               cls <- classes
               return $ hsep 0.5 $ map drawKnotDef cls
-}

        let failed = filter (\ cls ->
                    let invs = sort $ map invariantSet cls
                    in any (/= head invs) invs
                ) classes

        unless (null failed) $ do
            withFile "vlinks/failed_invariants.txt" WriteMode $ \ handle ->
                forM_ failed $ \ cls -> do
                    forM_ cls $ \ link -> do
                        hPrint handle $ minimalKauffmanXPolynomial link
                        when (numberOfVertices link > 0) $ do
                            let (t, _) = splitIntoTangleAndStar link
                            hPrint handle $ kauffmanXPolynomial t
                        hPutStrLn handle ""
                    hPutStrLn handle ""
                    hPutStrLn handle ""

            renderSVG "vlinks/failed.svg" (mkSizeSpec $ V2 (Just 512) Nothing) $ pad 1.05 $
                vsep 0.6 $ do
                   cls <- failed
                   return $ hsep 0.5 $ map drawKnotDef cls

    let sifted =
            siftByInvariant invariantSet $
                equivalenceClasses
                    (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink])
                    (forM_ diagrams)

    printTable "Embedded Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    unless (null $ collisionClasses sifted) $ do
        let classes = map (map representative) $ collisionClasses sifted

        renderSVG "vlinks/collisions.svg" (mkSizeSpec $ V2 (Just 512) Nothing) $ pad 1.05 $
            vsep 0.5 $ do
                cls <- classes
                return $ hsep 0.2 $ do
                    link <- cls
                    return $ drawKnotDef link

        forM_ (classes `zip` [1 :: Int ..]) $ \ (cls, i) -> do
            withFile (printf "vlinks/collision_%i.txt" i) WriteMode $ \ handle ->
                mapM_ (hPrint handle . edgeIndicesEncoding) cls
            withFile (printf "vlinks/collision_poly_%i" i) WriteMode $ \ handle ->
                mapM_ (hPrint handle . minimalKauffmanXPolynomial) cls

    do
        let group' f = groupBy (on (==) f) . sortBy (comparing f)

            paginate _ [] = []
            paginate n ls = let (h, t) = splitAt n ls
                            in h : paginate n t

            links = filter ((> 0) . numberOfVertices) $
                        mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted

        renderSVG "vlinks/links.svg" (mkSizeSpec $ V2 (Just 512) Nothing) $ pad 1.05 $
            vsep 5 $ do
                byThreads <- group' numberOfThreads links
                return $ hsep 3 $ do
                    byCross <- group' numberOfVertices byThreads
                    return $ hsep 0.5 $ do
                        page <- paginate 8 byCross
                        return $ vsep 0.5 $ do
                            link <- page
                            return $ drawKnotDef link

        forM_ (group' numberOfThreads links) $ \ byThreads ->
            forM_ (group' numberOfVertices byThreads) $ \ list -> do
                let n = numberOfVertices $ head list
                    k = numberOfThreads $ head list
                withFile (printf "vlinks/links_%i^%i.txt" n k) WriteMode $ \ handle ->
                    mapM_ (hPrint handle . edgeIndicesEncoding) list
                withFile (printf "vlinks/poly_%i^%i" n k) WriteMode $ \ handle ->
                    mapM_ (hPrint handle . minimalKauffmanXPolynomial) list
