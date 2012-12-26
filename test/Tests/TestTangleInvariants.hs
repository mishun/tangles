module Tests.TestTangleInvariants
	( tests
	) where

import Control.Monad (forM_)
import Test.HUnit
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.TwistedDouble
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.Skein.JonesPolynomial


testInvariantness n f =
	forM_ classes $ \ cls -> do
		let inv = map f cls
		mapM_ (@?= head inv) inv
	where
		classes = map allDiagrams $ tangleClasses $ \ yield ->
			simpleIncrementalGenerator
				(triangleBoundedType n primeIrreducibleDiagramType)
				[ArbitraryCrossing]
				n
				(\ t _ -> yield t)


tests = "Tangle invariants" ~: 
	[ "Linking numbers" ~:
		testInvariantness 6 linkingNumbersOfTangle

	, "Jones polynomial" ~:
		testInvariantness 6 minimalJonesPolynomialOfTangle

	, "Jones polynomial of doubling" ~:
		testInvariantness 3 (minimalJonesPolynomialOfTangle . twistedDouble)
	]
