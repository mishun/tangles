module Main where

import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import Control.Monad (when, forM_)
import Text.Printf
import System.Environment (getArgs)
import Diagrams.Prelude
import Math.Topology.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Draw
import TestUtil.Table
import TestUtil.Drawing


main :: IO ()
main = do
    [maxUp, maxL, maxN] <- do
        args <- getArgs
        return $ case args of
            [_]    -> 0 : 4 : map read args
            [_, _] -> 0 : map read args
            _      -> map read args

    let sifted = lookingForwardTanglesEnumeration False maxL maxUp maxN
    printTable "Tangles" $ generateTable'
        (numberOfVertices &&& numberOfLegs)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)

    when (length (collisionClasses sifted) > 0) $
        writeSVGImage (printf "tangles-collisions-%i.svg" maxN) (Width 500) $ pad 1.05 $
            vcat' with { _sep = 0.5 } $ do
                cls <- map (map representative) $ collisionClasses sifted
                return $ hcat' with { _sep = 0.2 } $ do
                    link <- cls
                    return $ drawKnotDef link
