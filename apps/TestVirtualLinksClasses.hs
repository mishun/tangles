module Main where

import Data.Ord (comparing)
import Data.List (sort, sortBy, nub)
import Control.Monad (unless, forM_, guard)
import Text.Printf
import System.Environment (getArgs)
import Diagrams.Prelude
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, listChordDiagrams, genusOfChordDiagram)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TestPrime
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.Topology.KnotTh.Moves.MovesOfELink
import Math.Topology.KnotTh.Draw
import TestUtil.Drawing


main :: IO ()
main = do
    [targetGenus, maxN] <- fmap (map read) getArgs

    let diagrams =
            filter (\ link -> not (isReidemeisterReducible link) && testPrime link) $
                tangleStarGlue
                    (filter ((== targetGenus) . genusOfChordDiagram . fst) . listChordDiagrams . generateNonPlanarRaw)
                    (forCCP_ $ primeIrreducibleDiagrams maxN)

    let classes = map (sortBy (comparing numberOfVertices) . allDiagrams) $
            equivalenceClasses (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink]) $
                forM_ diagrams

    forM_ classes $ \ cls -> do
            let invs = sort $ map minimalKauffmanXPolynomial cls
            unless (all (== head invs) invs) $
                putStrLn $ "Class failed: " ++ show (nub invs)

    writeSVGImage (printf "virtual-link-classes-%i-%i.svg" targetGenus maxN) (Width 500) $ pad 1.05 $
        vcat' with { _sep = 0.6 } $ do
            cls <- classes
            guard $ any (\ l -> numberOfVertices l <= 4 && numberOfThreads l == 1) cls
            return $ hcat' with { _sep = 0.5 } $ do
                link <- cls
                return $ drawKnotDef link
