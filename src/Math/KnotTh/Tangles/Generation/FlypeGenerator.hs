module Math.KnotTh.Tangles.Generation.FlypeGenerator
	( generateFlypeEquivalentDecomposition
	, generateFlypeEquivalent
	) where

import Data.List (nubBy)
import Data.Array ((!), (//), listArray)
import Control.Monad.State.Strict (evalStateT, get, put, lift)
import Control.Monad (forM_)
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (DnSubGroup, maximumSubGroup, addSymmetryToSubGroup)
import Math.KnotTh.Crossings
import Math.KnotTh.Crossings.SubTangle
import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.Flypes
import Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangles.BorderIncremental.RootingTest (rootCodeLeg, minimumRootCode)
import Math.KnotTh.Tangles.BorderIncremental.IncrementalTests


generateFlypeEquivalentDecomposition :: (Monad m) => Int -> (SubTangleTangle ProjectionCrossing -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalentDecomposition maxN yield = flip evalStateT (0, listArray (1, maxN) $ repeat []) $ do

	let templateType = GluingType
		{ preGlueTest  = \ _ leg gl ->
			let t = dartTangle leg
			in case () of
				_ | numberOfCrossings t == 1 -> gl < 2
				  | numberOfLegs t == 4      -> False
				  | otherwise                -> testMultiEdges leg gl
		, postGlueTest = \ root gl _ s ->
			if gl < 3 || testFlow4 root
				then return $! s
				else Nothing
		}

	let directSumType = GluingType
		{ preGlueTest  = \ cr leg gl ->
			let	t = dartTangle leg
				lp = directSumDecompositionTypeById cr 0
				rp = directSumDecompositionType (opposite leg)
			in case () of
				_ | numberOfLegs t /= 4      -> False
				  | gl /= 2                  -> False
				  | testMultiEdges leg gl    -> False
				  | lp == DirectSum01_23     -> False
				  | numberOfCrossings t == 1 -> rp /= DirectSum12_30
				  | otherwise                -> True
		, postGlueTest = \ root _ leg s ->
			let	coLeg = nextCCW $ nextCCW leg
				flypeS =
					case additionalFlypeSymmetry (crossingTangle root) of
						Just x  -> addSymmetryToSubGroup s x
						Nothing -> s
			in case (isLoner' root, isLoner' $ incidentCrossing $ opposite leg, isLoner' $ incidentCrossing $ opposite coLeg) of
				(False, False, False) -> return $! s
				(True , True , _    ) -> return $! flypeS
				(True , False, False) ->
					if min (rootCodeLeg leg ccw) (rootCodeLeg (nextCW leg) cw) <= min (rootCodeLeg coLeg ccw) (rootCodeLeg (nextCW coLeg) cw)
						then return $! flypeS
						else Nothing
				_                     -> Nothing
		}

	let growTree rootTemplate rootSymmetry = do
		(root, crossings, rootN) <- do
			(!free, !prev) <- get
			let cr =
				let sumType
					| (a == b) && (c == d) && (a /= c)  = DirectSum01_23
					| (b == c) && (a == d) && (a /= b)  = DirectSum12_30
					| otherwise                         = NonDirectSumDecomposable
					where
						[a, b, c, d] = map adjacentCrossing $ allLegs rootTemplate
				in fromTangle' rootTemplate rootSymmetry sumType free
			let n = numberOfCrossings (subTangle cr)
			let root = lonerTangle $ crossing' cr

			let next
				| n >= maxN - 1  = prev
				| otherwise      = prev // [(n, cr : (prev ! n))]

			put $! next `seq` (free + 1, next)
			return $! (root, next, n)

		let glueTemplates curN ancestor ancestorSymmetry =
			forM_ [1 .. maxN - curN] $ \ cn -> do
				let desc = canonicalGluing templateType $ representativeGluingSites (crossings ! cn) (ancestor, ancestorSymmetry)
				forM_ desc $ \ (child, childSymmetry) ->
					case numberOfLegs child of
						4 -> growTree child childSymmetry
						_ -> do
							lift $ yield child childSymmetry
							glueTemplates (curN + cn) child childSymmetry

		let glueDirectSums curN ancestor _ =
			forM_ [1 .. maxN - curN] $ \ cn -> do
				let desc = nubBy (\ (a, _) (b, _) -> minimumRootCode a == minimumRootCode b) $
					canonicalGluing directSumType $ allGluingSites' (crossings ! cn) 2 ancestor
				forM_ desc $ \ (child, childSymmetry) -> do
					growTree child childSymmetry
					glueDirectSums (curN + cn) child childSymmetry

		lift $ yield rootTemplate rootSymmetry
		glueTemplates rootN root rootSymmetry
		glueDirectSums rootN root rootSymmetry

	let lonerSymmetry = maximumSubGroup 4
	let loner = crossing' $ fromTangle lonerProjection lonerSymmetry NonDirectSumDecomposable 0
	growTree (lonerTangle loner) lonerSymmetry


generateFlypeEquivalent :: (Monad m) => Int -> (TangleProjection -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalent maxN yield = generateFlypeEquivalentDecomposition maxN (\ t s -> yield (substitute t) s)
