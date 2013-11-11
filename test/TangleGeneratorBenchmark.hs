{-# LANGUAGE Rank2Types #-}
module Main (main) where

import Control.Monad.State (execState, modify)
import Control.Monad (when)
import Diagrams.Prelude
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
import Math.Topology.KnotTh.Tangle.Generation.FlypeClasses
import TestUtil.Table
import TestUtil.Drawing


main :: IO ()
main = do
    printTable "Prime projections"    $ generateTable $ forCCP_ (primeProjections 8)
    printTable "Template projections" $ generateTable $ forCCP_ (templateProjections 9)
    printTable "Alternating tangles"  $ generateTable $ generateFlypeEquivalent 8
    printTable "Prime diagrams"       $ generateTable $ forCCP_ (primeIrreducibleDiagrams 6)

    writeSVGImage "tangles.svg" (Width 250) $ pad 1.05 $ flip execState mempty $
        forCCP_ (primeIrreducibleDiagrams 3) $ \ (tangle, _) ->
            when (numberOfLegs tangle == 4) $
                modify (=== pad 1.1 (drawKnotDef tangle))
