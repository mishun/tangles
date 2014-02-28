module Main (main) where

import qualified Data.Map as M
import Control.Monad.Writer (execWriter, tell)
import Control.Monad (forM_)
import Diagrams.Prelude
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Link.Table
import Math.Topology.KnotTh.Tabulation.LinkDiagrams
import Math.Topology.KnotTh.Draw
import TestUtil.Drawing


main :: IO ()
main = do
    let walk n l | numberOfVertices l >= n  = [l]
                 | otherwise                =
            l : concatMap (walk n) (nextGeneration projectionCrossings l)

    let t = foldl (\ m l -> M.insertWith (++) (numberOfVertices l) [l] m) M.empty
                (walk 6 $ projection hopfLink)

    writeSVGImage "links.svg" (Width 500) $ pad 1.05 $ execWriter $
        forM_ (M.elems t `zip` [0 ..]) $ \ (cc, j) ->
            forM_ (cc `zip` [0 ..]) $ \ (link, i) ->
                tell $ translate (r2 (2.2 * i, -2.2 * j)) $ drawKnotDef link

