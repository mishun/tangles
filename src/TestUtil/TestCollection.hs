module TestUtil.TestCollection
    ( tests
    ) where

import Test.Framework (Test)
import qualified Math.Combinatorics.ChordDiagrams.Test as TestChordDiagrams
import qualified Math.KnotTh.Invariants.Test as TestInvariants
import qualified Math.KnotTh.Tangle.BorderIncremental.Test as TestTangleGenerators
import qualified Math.KnotTh.Enumeration.Applied.Test as TestTangleEnumeration
import qualified Math.KnotTh.Tangle.Test as TestBasicTangle
import qualified Math.Combinatorics.Strings.Test as TestCombinatoricsStrings
import qualified Math.KnotTh.SurfaceLink.Test as TestSurfaceLink


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
