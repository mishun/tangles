module Main where

import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Text.Printf
import System.Environment (getArgs)
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
import TestUtil.Table


main :: IO ()
main = do
    [maxN] <- fmap (map read) getArgs

    let diagrams =
            filter (\ link -> not (isReidemeisterReducible link) && testPrime link) $
                tangleStarGlue
                    (filter ((== 1) . genusOfChordDiagram . fst) . listChordDiagrams . generateNonPlanarRaw)
                    (forCCP_ $ primeIrreducibleDiagrams maxN)

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
