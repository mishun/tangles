module Main where

import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Control.Arrow ((&&&))
import Control.Monad (when, forM_)
import Text.Printf
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), hPrint, hPutStrLn)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Math.Topology.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.EdgeIndicesEncoding
import TestUtil.Table


main :: IO ()
main = do
    [maxUp, maxL, maxN] <- do
        args <- getArgs
        return $ case args of
            [_]    -> 0 : 4 : map read args
            [_, _] -> 0 : map read args
            _      -> map read args

    let sifted = lookingForwardWeakTanglesEnumeration True maxL maxUp maxN
    printTable "Weak Tangles" $ generateTable'
        (numberOfVertices &&& numberOfLegs)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    let group' f = groupBy (on (==) f) . sortBy (comparing f)

    do
        let w = mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted

        withFile "weak-codes.txt" WriteMode $ \ handle ->
            mapM_ (hPrint handle . edgeIndicesEncoding) $
                sortBy (comparing $ numberOfVertices &&& numberOfThreads) w

        renderSVG "weak.svg" (Width 512) $
            vcat' with { _sep = 2 } $ do
                tc <- group' numberOfVertices w
                return $ vcat' with { _sep = 0.2 } $ do
                    tt <- group' numberOfThreads tc
                    return $ hcat' with { _sep = 0.2 } $ map drawKnotDef tt

    when (length (collisionClasses sifted) > 0) $ do
        withFile "weak-collisions-codes.txt" WriteMode $ \ handle ->
            forM_ (map (map representative) $ collisionClasses sifted) $ \ cls -> do
                mapM_ (hPrint handle . edgeIndicesEncoding) cls
                hPutStrLn handle ""

        renderSVG "weak-collisions.svg" (Width 512) $ pad 1.05 $
            vcat' with { _sep = 0.5 } $ do
                cls <- map (map representative) $ collisionClasses sifted
                return $ hcat' with { _sep = 0.2 } $ do
                    link <- cls
                    return $ drawKnotDef link
