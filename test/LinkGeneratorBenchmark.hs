module Main (main) where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Control.Monad.Writer (execWriter, tell)
import Control.Monad (forM_)
import Text.Printf
import Diagrams.Prelude
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Tabulation.LinkDiagrams
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tangle.Satellites
import Math.Topology.KnotTh.Moves.Moves
import Math.Topology.KnotTh.Moves.PatternMatching
import qualified Math.Topology.KnotTh.Moves.AdHoc as AdHoc
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Enumeration.DiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import TestUtil.Table
import TestUtil.Drawing


main :: IO ()
main = do
    let diagramsGen maxN =
            let walk link | numberOfVertices link >= maxN  = p
                          | otherwise                      = p ++ next
                    where
                        p = [link | not (isReidemeisterReducible link)]
                        next = concatMap walk (nextGeneration bothDiagramCrossings link)
            in forM_ (walk hopfLink)

        linkClasses maxN =
            siftByInvariant
                (\ l -> ( minimalJonesPolynomial l
                        , minimalKauffmanFPolynomial l
                        , minimalJonesPolynomial $ tangleToLink $ twistedDoubleSatellite $ linkToTangle l
                        )
                ) $
                equivalenceClasses
                    --[map AdHoc.greedyReidemeisterReduction . searchMoves [flype, pass1, pass2, pass3]]
                    (map (map AdHoc.greedyReidemeisterReduction .) [AdHoc.reidemeisterIII, AdHoc.flype, AdHoc.pass])
                    (diagramsGen maxN)

    let n = 10

    printTable "Diagrams" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (diagramsGen n)

    let sifted = linkClasses n
    printTable "Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)
    writeSVGImage "collisions.svg" (Width 500) $ pad 1.05 $ execWriter $
        forM_ (collisionClasses sifted `zip` [0 ..]) $ \ (cc, j) ->
            forM_ (cc `zip` [0 ..]) $ \ (info, i) ->
                tell $ translate (r2 (2.2 * i, -2.2 * j)) $ drawKnotDef $ representative info
