module Main (main) where

import Test.HUnit
import qualified Math.Combinatorics.ChordDiagrams.Test as TestChordDiagrams
import qualified Math.KnotTh.Invariants.Test as TestInvariants
import qualified Math.KnotTh.Tangle.BorderIncremental.Test as TestTangleGenerators
import qualified Math.KnotTh.Enumeration.Applied.Test as TestTangleInvariants
import qualified Math.KnotTh.Tangle.Test as TestBasicTangle


tests = test
    [ TestBasicTangle.tests
    , TestChordDiagrams.tests
    , TestTangleGenerators.tests
    , TestInvariants.tests
    , TestTangleInvariants.tests
    ]


main = runTestTT tests
