module Main (main) where

import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Control.Monad (when, forM_)
import Text.Printf
import System.Environment (getArgs)
import Diagrams.Prelude
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.LinkDiagrams
import Math.Topology.KnotTh.Draw
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
                    --[map reidemeisterReduction . searchMoves [flype, pass1, pass2, pass3]]
                    (map (map reidemeisterReduction .) [reidemeisterIII, AdHoc.flype, AdHoc.pass])
                    (diagramsGen maxN)

    [maxN] <- fmap (map read) getArgs

    printTable "Diagrams" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (diagramsGen maxN)

    let sifted = linkClasses maxN
    printTable "Links" $ generateTable'
        (numberOfVertices &&& numberOfThreads)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    when (length (collisionClasses sifted) > 0) $
        writeSVGImage (printf "links-collisions-%i.svg" maxN) (Width 500) $ pad 1.05 $
            vcat' with { _sep = 0.5 } $ do
                cls <- map (map representative) $ collisionClasses sifted
                return $ hcat' with { _sep = 0.2 } $ do
                    link <- cls
                    return $ drawKnotDef link <> strutX 2 <> strutY 2
