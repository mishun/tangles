module Main (main) where

import Test.Framework (defaultMain)
import qualified Math.Topology.KnotTh.ChordDiagram.Test
import qualified Math.Topology.KnotTh.Cobordism.CobordismMatrix.Test
import qualified Math.Topology.KnotTh.Cobordism.DottedCobordism.Test
import qualified Math.Topology.KnotTh.EmbeddedLink.Test
import qualified Math.Topology.KnotTh.Enumeration.Applied.Test
import qualified Math.Topology.KnotTh.Invariants.KhovanovHomology.Test
import qualified Math.Topology.KnotTh.Invariants.Test
import qualified Math.Topology.KnotTh.Tabulation.Test
import qualified Math.Topology.KnotTh.Tangle.Test


main :: IO ()
main =
    defaultMain
        [ Math.Topology.KnotTh.Cobordism.DottedCobordism.Test.test
        , Math.Topology.KnotTh.Cobordism.CobordismMatrix.Test.test
        , Math.Topology.KnotTh.EmbeddedLink.Test.test
        , Math.Topology.KnotTh.Tangle.Test.test
        , Math.Topology.KnotTh.Invariants.KhovanovHomology.Test.test
        , Math.Topology.KnotTh.Invariants.Test.test
        , Math.Topology.KnotTh.ChordDiagram.Test.test
        , Math.Topology.KnotTh.Tabulation.Test.test
        , Math.Topology.KnotTh.Enumeration.Applied.Test.test
        ]
