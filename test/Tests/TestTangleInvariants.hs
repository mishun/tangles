module Tests.TestTangleInvariants
	( tests
	) where

import Control.Monad (forM_)
import Test.HUnit
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Invariants.LinkingNumber


classes n = map allDiagrams $ tangleClasses $ \ yield ->
	simpleIncrementalGenerator
		(triangleBoundedType n primeIrreducibleDiagramType)
		[ArbitraryCrossing]
		n
		(\ t _ -> yield t)


tests = "Tangle invariants" ~: 
	[ "Linking numbers" ~: do
		forM_ (classes 6) $ \ cls -> do
			let inv = map linkingNumbersOfTangle cls
			mapM_ (@?= head inv) inv
	]
