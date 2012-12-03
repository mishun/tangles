module Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
	( GluingType(..)
	, nextNumberOfLegs
	, diagonalIndex
	, allGluingSites'
	, allGluingSites
	, representativeGluingSites'
	, representativeGluingSites
	, canonicalGluing
	, simpleIncrementalGenerator
	) where

import Control.Monad (when, guard)
import Math.Algebra.Group.Dn (DnSubGroup, pointsUnderSubGroup, rotationPeriod, hasReflectionPart, mirroredZero)
import Math.Algebra.Group.D4 (e, ec2, ec3, toDnSubGroup)
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.BorderIncremental.RootingTest


data GluingType ct s = GluingType
	{ preGlueTest  :: CrossingState ct -> Dart ct -> Int -> Bool
	, postGlueTest :: Crossing ct -> Int -> Dart ct -> DnSubGroup -> Maybe s
	}


{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2


allGluingSites' :: (CrossingType ct) => [ct] -> Int -> Tangle ct -> [(Int, Dart ct, CrossingState ct)]
allGluingSites' crossingsToGlue !gl !tangle = do
	!cr <- crossingsToGlue
	!leg <- allLegs tangle
	!state <- possibleOrientations cr Nothing
	return $! (gl, leg, state)


allGluingSites :: (CrossingType ct) => [ct] -> Tangle ct -> [(Int, Dart ct, CrossingState ct)]
allGluingSites crossingsToGlue tangle = do
	let l = numberOfLegs tangle
	gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
	allGluingSites' crossingsToGlue gl tangle


representativeGluingSites' :: (CrossingType ct) => [ct] -> Int -> (Tangle ct, DnSubGroup) -> [(Int, Dart ct, CrossingState ct)]
representativeGluingSites' crossingsToGlue !gl (!tangle, !symmetry)
	| numberOfLegs tangle /= pointsUnderSubGroup symmetry  = error "gluingSites: different orders"
	| otherwise                                            = do
		let period = rotationPeriod symmetry

		(!legIndex, !inducedSymmetry) <-
			if not $ hasReflectionPart symmetry
				then [(x, Nothing) | x <- [0 .. period - 1]]
				else do
					let mz = mod ((mirroredZero symmetry) + gl - 1) period

					let getEndpoint doubleIndex = (legIndex, induced)
						where
							legIndex = doubleIndex `quot` 2

							induced
								| even doubleIndex  = Just $! {- fixup <*> -} case gl of { 3 -> ec2 ; 2 -> ec3 ; _ -> e }
								| otherwise         = Nothing

							--gr = Dn.D $ numberOfLegs tangle
							--fixup = Dn.adjointDifferenceForBasis (globalTransformBasis tangle) symmetry
							--		(Dn.fromRotationReflection gr (legIndex - gl + 1, True))
							--		(Dn.fromRotation gr legIndex)

					let left = getEndpoint (mz - period)
					let right = getEndpoint mz

					let fill !c
						| c == fst right  = [right] -- sic!
						| c == fst left   = left : fill (c + 1)
						| otherwise       = (c, Nothing) : fill (c + 1)

					fill $! fst left

		let !leg = nthLeg tangle legIndex
		!cr <- crossingsToGlue
		!state <- possibleOrientations cr inducedSymmetry
		return $! (gl, leg, state)


representativeGluingSites :: (CrossingType ct) => [ct] -> (Tangle ct, DnSubGroup) -> [(Int, Dart ct, CrossingState ct)]
representativeGluingSites crossingsToGlue ts@(tangle, _) = do
	let l = numberOfLegs tangle
	gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
	representativeGluingSites' crossingsToGlue gl ts


canonicalGluing :: (CrossingType ct) => GluingType ct s -> [(Int, Dart ct, CrossingState ct)] -> [(Tangle ct, s)]
canonicalGluing gluing sites = do
	(!gl, !leg, !st) <- sites
	guard $ preGlueTest gluing st leg gl
	let root = glueToBorder leg gl st
	case rootingTest root >>= postGlueTest gluing root gl leg of
		Nothing -> []
		Just r  -> return $! (crossingTangle root, r)


simpleIncrementalGenerator :: (Monad m, CrossingType ct) => GluingType ct DnSubGroup -> [ct] -> Int -> (Tangle ct -> DnSubGroup -> m ()) -> m ()
simpleIncrementalGenerator gluing crossingsToGlue maxN yield =
	let dfs node@(!tangle, !symmetry) = do
		yield tangle symmetry
		when (numberOfCrossings tangle < maxN) $
			mapM_ dfs $ canonicalGluing gluing $ representativeGluingSites crossingsToGlue node
	in mapM_ (dfs . makeRoot . makeCrossing') crossingsToGlue
	where
		makeRoot :: (CrossingType ct) => CrossingState ct -> (Tangle ct, DnSubGroup)
		makeRoot st = (lonerTangle st, toDnSubGroup $ localCrossingSymmetry $ crossingType st)
