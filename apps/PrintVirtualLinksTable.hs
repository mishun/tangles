module Main where

import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Control.Monad (when, forM_)
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
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Moves.MovesOfELink
import Math.Topology.KnotTh.Draw
import TestUtil.Table
import TestUtil.Drawing


main :: IO ()
main = do
    [maxN] <- fmap (map read) getArgs

    let diagrams =
            filter (\ link -> not (isReidemeisterReducible link) && testPrime link) $
                tangleStarGlue
                    (filter ((== 1) . genusOfChordDiagram . fst) . listChordDiagrams . generateNonPlanarRaw)
                    (forCCP_ $ primeIrreducibleDiagrams maxN)

    let sifted =
            siftByInvariant minimalKauffmanXPolynomial $
                equivalenceClasses
                    (map (map reidemeisterReduction .) [reidemeisterIII, movesOfELink])
                    (forM_ diagrams)

    printTable "Embedded Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    when (length (collisionClasses sifted) > 0) $ do
        writeSVGImage (printf "torus-links-collisions-%i.svg" maxN) (Width 500) $ pad 1.05 $
            vcat' with { _sep = 0.5 } $ do
                cls <- map (map representative) $ collisionClasses sifted
                return $ hcat' with { _sep = 0.2 } $ do
                    link <- cls
                    return $ drawKnotDef link
