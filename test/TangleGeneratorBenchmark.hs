module Main (main) where

import Control.Monad.State
import Control.Monad
import Diagrams.Prelude
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Draw
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator
import TestUtil.Table
import TestUtil.Drawing


main :: IO ()
main = do
    printTable "Prime projections"    $ generateTable False $ simpleIncrementalGenerator primeProjectionType [ProjectionCrossing] 8
    printTable "Template projections" $ generateTable False $ simpleIncrementalGenerator templateProjectionType [ProjectionCrossing] 9
    printTable "Alternating tangles"  $ generateTable False $ generateFlypeEquivalent 8
    printTable "Prime diagrams"       $ generateTable False $ simpleIncrementalGenerator primeDiagramType [ArbitraryCrossing] 6

    writeSVGImage "tangles.svg" (Width 250) $ pad 1.05 $ flip execState mempty $
        simpleIncrementalGenerator primeProjectionType [ProjectionCrossing] 5 $ \ tangle _ ->
            when (numberOfLegs tangle == 4) $
                modify (=== pad 1.1 (drawKnotDef tangle))
