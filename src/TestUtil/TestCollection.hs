module TestUtil.TestCollection
    ( tests
    ) where

import Test.Framework (Test)
import qualified Math.Combinatorics.ChordDiagram.Test as TestChordDiagrams
import qualified Math.Topology.KnotTh.Invariants.Test as TestInvariants
import qualified Math.Topology.KnotTh.Tabulation.Test as TestTabulation
import qualified Math.Topology.KnotTh.Enumeration.Applied.Test as TestTangleEnumeration
import qualified Math.Topology.KnotTh.Tangle.Test as TestBasicTangle
import qualified Math.Combinatorics.Strings.Test as TestCombinatoricsStrings
import qualified Math.Topology.KnotTh.EmbeddedLink.Test as TestEmbeddedLink


tests :: [Test]
tests =
    [ TestCombinatoricsStrings.test
    , TestEmbeddedLink.test
    , TestBasicTangle.test
    , TestInvariants.test
    , TestChordDiagrams.test
    , TestTabulation.test
    , TestTangleEnumeration.test
    ]
