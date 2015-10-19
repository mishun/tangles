{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (when, forM_, unless)
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hPrint, hPutStrLn)
import Text.Printf
import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Math.Topology.KnotTh.Moves.AdHoc as AdHoc
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.Topology.KnotTh.Enumeration.EquivalenceClasses
import Math.Topology.KnotTh.Enumeration.SiftByInvariant
import Math.Topology.KnotTh.Invariants
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Tangle
import TestUtil.Table


tangleDiagrams :: (Monad m) => Int -> Int -> (TangleDiagram -> m ()) -> m ()
tangleDiagrams legsLimit maxN yield =
    forCCP_ (primeIrreducibleDiagrams maxN) $ \ (tangle, _) ->
        when (legsLimit < 0 || numberOfLegs tangle == legsLimit) $
            yield tangle


tangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (TangleDiagram -> m ()) -> m ()) -> [info TangleDiagram]
tangleClasses =
    equivalenceClasses
        [ map reidemeisterReduction . reidemeisterIII
        , map reidemeisterReduction . AdHoc.flype
        , map reidemeisterReduction . AdHoc.pass
        -- , AdHoc.weak
        ]


main :: IO ()
main = do
    [forward, maxLegs, maxN] <- do
        args <- getArgs
        return $ case args of
            [_]    -> 0 : 4 : map read args
            [_, _] -> 0 : map read args
            _      -> map read args

    let invariantSet tangle = ( minimalKauffmanXPolynomial tangle
                              , minimalKauffmanXPolynomial $ twistedSatellite 2 tangle
                              -- , minimalKauffmanFPolynomial $ twistedSatellite 3 tangle
                              -- , kauffmanFPolynomial $ tangleDoublingLink id tangle
                              )

    do
        let classes = map (sortBy (comparing numberOfVertices) . allDiagrams) $
                        tangleClasses $ tangleDiagrams maxLegs maxN

        let failed = filter (\ cls ->
                    let invs = sort $ map minimalKauffmanXPolynomial cls
                    in any (/= head invs) invs
                ) classes

        unless (null failed) $ do
            withFile "tangles/failed_invariants.txt" WriteMode $ \ handle ->
                forM_ failed $ \ cls -> do
                    forM_ cls $ \ tangle -> do
                        --hPrint handle $ minimalKauffmanXPolynomial tangle
                        hPrint handle $ unrootedHomeomorphismInvariant tangle
                    hPutStrLn handle ""

            renderSVG "tangles/failed.svg" (mkSizeSpec $ V2 (Just 512) Nothing) $ pad 1.05 $
                vsep 0.6 $ map (hsep 0.5 . map drawKnotDef) failed

    let sifted = siftByInvariant invariantSet $ filter ((<= maxN) . numberOfVertices . representative) $
                    tangleClasses $ tangleDiagrams maxLegs (maxN + forward)

    printTable "Tangles" $ generateTable'
        (numberOfVertices &&& numberOfLegs)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    unless (null $ collisionClasses sifted) $
        renderSVG (printf "tangles/collisions-%i.svg" maxN) (mkSizeSpec $ V2 (Just 512) Nothing) $ pad 1.05 $
            vsep 0.5 $ do
                cls <- map (map representative) $ collisionClasses sifted
                return $ hsep 0.2 $ map drawKnotDef cls
