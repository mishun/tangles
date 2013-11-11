module TestUtil.TestCollection
    ( tests
    ) where

import Test.Framework (Test)
import qualified Math.Combinatorics.ChordDiagram.Test as TestChordDiagrams
import qualified Math.Topology.KnotTh.Invariants.Test as TestInvariants
import qualified Math.Topology.KnotTh.Tangle.BorderIncremental.Test as TestTangleGenerators
import qualified Math.Topology.KnotTh.Enumeration.Applied.Test as TestTangleEnumeration
import qualified Math.Topology.KnotTh.Tangle.Test as TestBasicTangle
import qualified Math.Combinatorics.Strings.Test as TestCombinatoricsStrings
import qualified Math.Topology.KnotTh.SurfaceLink.Test as TestSurfaceLink


tests :: [Test]
tests =
    [ TestCombinatoricsStrings.test
    , TestSurfaceLink.test
    , TestBasicTangle.test
    , TestChordDiagrams.test
    , TestTangleGenerators.test
    , TestInvariants.test
    , TestTangleEnumeration.test
    ]
