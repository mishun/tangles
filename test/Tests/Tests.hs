module Tests.Tests
	( tests
	) where

import Test.HUnit
import qualified Tests.TestChordDiagrams
import qualified Tests.TestLinkInvariants
import qualified Tests.TestTangleGenerators
import qualified Tests.TestTangleInvariants


tests = test
	[ Tests.TestChordDiagrams.tests
	, Tests.TestTangleGenerators.tests
	, Tests.TestLinkInvariants.tests
	, Tests.TestTangleInvariants.tests
	]
