module Main (main) where

import Test.Framework (defaultMain)
import qualified Math.Combinatorics.ChordDiagram.Test
import qualified Math.Combinatorics.Strings.Test
import qualified Math.Topology.KnotTh.EmbeddedLink.Test
import qualified Math.Topology.KnotTh.Enumeration.Applied.Test
import qualified Math.Topology.KnotTh.Invariants.Test
import qualified Math.Topology.KnotTh.Tabulation.Test
import qualified Math.Topology.KnotTh.Tangle.Test


main :: IO ()
main =
    defaultMain
        [ Math.Combinatorics.Strings.Test.test
        , Math.Topology.KnotTh.EmbeddedLink.Test.test
        , Math.Topology.KnotTh.Tangle.Test.test
        , Math.Topology.KnotTh.Invariants.Test.test
        , Math.Combinatorics.ChordDiagram.Test.test
        , Math.Topology.KnotTh.Tabulation.Test.test
        , Math.Topology.KnotTh.Enumeration.Applied.Test.test
        ]
