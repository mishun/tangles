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
    do
        let table = foldl (\ m l -> M.insertWith (++) (numberOfVertices l) [l] m) M.empty $
                let walk 0 link = [link]
                    walk depth link =
                        link : concatMap (walk $ depth - 1) (nextGeneration projectionCrossings link)
                in walk 10 $ projection hopfLink

        print $ M.toList $ M.map length table

    writeSVGImage "links-tree.svg" (Width 500) $ pad 1.05 $
        let walk 0 link = drawKnotDef link
            walk depth link =
                let next = nextGeneration projectionCrossings link
                in drawKnotDef link === pad 1.1 (hcat' with { _sep = 0.1 } $ map (walk $ depth - 1) next)
        in walk 5 $ projection hopfLink
