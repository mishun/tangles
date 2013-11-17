{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Data.Maybe (mapMaybe)
import Control.Monad.Writer (execWriter, tell)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Text.Printf
import Diagrams.Prelude
import Math.Topology.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.Topology.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tangle.Moves.Test
import Math.Topology.KnotTh.Link.Table
import TestUtil.Table
import TestUtil.Drawing
import Math.Topology.KnotTh.Tangle.NonAlternating.Satellites


main :: IO ()
main = do
    --printTable "Diagrams" $ generateTable' $ tangleDiagrams False (-1) 5

    let sifted = lookingForwardTanglesEnumeration True 6 0 8
    printTable "Tangles" $ generateTable'
        (numberOfVertices &&& numberOfLegs)
        (const 1)
        (forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted))
    putStrLn $ printf "Collision classes: %i" (length $ collisionClasses sifted)
    writeSVGImage "collisions.svg" (Width 500) $ pad 1.05 $ execWriter $
        forM_ (collisionClasses sifted `zip` [0 ..]) $ \ (cc, j) ->
            forM_ (cc `zip` [0 ..]) $ \ (info, i) ->
                tell $ translate (r2 (2.2 * i, -2.2 * j)) $ drawKnotDef $ representative info
