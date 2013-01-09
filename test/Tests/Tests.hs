module Tests.Tests
	( tests
	) where

import Test.HUnit
import qualified Tests.TestChordDiagrams
import qualified Tests.TestInvariants
import qualified Tests.TestTangleGenerators
import qualified Tests.TestTangleInvariants
import qualified Tests.TestBasicTangle


tests = test
	[{- Tests.TestBasicTangle.tests
	, Tests.TestChordDiagrams.tests
	, Tests.TestTangleGenerators.tests
	, Tests.TestInvariants.tests
	,-} Tests.TestTangleInvariants.tests
	]
