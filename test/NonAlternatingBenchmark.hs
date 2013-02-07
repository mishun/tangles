{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Data.Array.Base (listArray)
import Data.Maybe (mapMaybe)
import Control.Monad.Writer
import Text.Printf
import Diagrams.Prelude
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Draw.DrawKnot
import Math.KnotTh.Link.FromTangle
import Math.KnotTh.Tangle.Table
import Math.KnotTh.Tangle.Moves.Test
import Math.KnotTh.Invariants.Skein.StateSum.TangleRelation
import Math.KnotTh.Link.Table
import TestUtil.Table
import TestUtil.Drawing


data Relation = Relation

instance SkeinRelation Relation Int where
    circleFactor _ = 0
    initialLplus _ = []
    twistPFactor _ = 1
    twistNFactor _ = 1
    smoothLplusFactor  = 1
    smoothLzeroFactor  = 1
    smoothLinftyFactor = 1
    finalNormalization _ _ = id


main :: IO ()
main = do
    printTable "Diagrams" $ generateTable' $ tangleDiagrams False (-1) 5

    let sifted = lookingForwardTanglesEnumeration True 6 0 6
    printTable "Tangles" $ generateTable' $ forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted)
    printf "Collision classes: %i" (length $ collisionClasses sifted)
    writeSVGImage "collisions.svg" (Width 500) $ pad 1.05 $ execWriter $
        forM_ (collisionClasses sifted `zip` [0 ..]) $ \ (cc, j) ->
            forM_ (cc `zip` [0 ..]) $ \ (info, i) ->
                tell $ translate (r2 (2.2 * i, -2.2 * j)) $ drawKnot defaultDraw $ representative info

    let ts = {- concatMap allOrientationsOfTangle -}
            [ altTriangleTangle
            , naTriangleTangle
            , rotateTangle 1 $ restoreBasicTangle $ listArray (0, 9) [5, 6, 7, 8, 9, 0, 1, 2, 3, 4]
            ]

    writeSVGImage "basis.svg" (Width 1000) $ pad 1.05 $ execWriter $
        forM_ (zip ts [0 ..]) $ \ (t, j) -> do
            tell $ translate (r2 (0, -2.2 * j)) $ drawKnot defaultDraw t
            forM_ (map extractTangle (decomposeTangle Relation 1 t) `zip` [0 ..]) $ \ (nt, i) ->
                tell $ translate (r2 (2.2 * i + 3, -2.2 * j)) $ drawKnot defaultDraw nt
