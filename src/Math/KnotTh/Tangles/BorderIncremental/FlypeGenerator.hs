module Math.KnotTh.Tangles.BorderIncremental.FlypeGenerator
	( generateFlypeEquivalentDecomposition
	, generateFlypeEquivalentDecompositionInTriangle
	, generateFlypeEquivalent
	, generateFlypeEquivalentInTriangle
	) where

import Data.List (nubBy)
import Data.Array ((!), (//), listArray)
import Data.Function (fix)
import Control.Monad.State.Strict (evalStateT, execStateT, get, put, lift)
import Control.Monad (forM_, when, unless)
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (DnSubGroup, maximumSubGroup, addSymmetryToSubGroup)
import Math.KnotTh.Crossings.SubTangle
import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.Flypes
import Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangles.BorderIncremental.RootingTest (rootCodeLeg, minimumRootCode)
import Math.KnotTh.Tangles.BorderIncremental.IncrementalTests


generateFlypeEquivalentDecomposition' :: (Monad m) => Bool -> Int -> (SubTangleTangle ProjectionCrossing -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalentDecomposition' triangle maxN yield = do

	let templateDescendants =
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
		in \ crossings -> canonicalGluing templateType . representativeGluingSites crossings

	let directSumDescendants =
		let directSumType = GluingType
			{ preGlueTest  = \ cr leg gl ->
				let	t = dartTangle leg
					lp = directSumDecompositionType cr
					rp = directSumDecompositionTypeInside (opposite leg)
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
				in case (isLonerInside root, isLonerInside $ incidentCrossing $ opposite leg, isLonerInside $ incidentCrossing $ opposite coLeg) of
					(False, False, False) -> return $! s
					(True , True , _    ) -> return $! flypeS
					(True , False, False) ->
						if min (rootCodeLeg leg ccw) (rootCodeLeg (nextCW leg) cw) <= min (rootCodeLeg coLeg ccw) (rootCodeLeg (nextCW coLeg) cw)
							then return $! flypeS
							else Nothing
					_                     -> Nothing
			}
		in \ crossings -> nubBy (\ (a, _) (b, _) -> minimumRootCode a == minimumRootCode b) .
			canonicalGluing directSumType . allGluingSites' crossings 2 . fst

	let buildCrossingType template symmetry =
		let sumType
			| (a == b) && (c == d) && (a /= c)  = DirectSum01_23
			| (b == c) && (a == d) && (a /= b)  = DirectSum12_30
			| otherwise                         = NonDirectSumDecomposable
			where
				[a, b, c, d] = map adjacentCrossing $ allLegs template
		in fromTangle' template symmetry sumType

	let halfN = maxN `div` 2

	(finalFree, finalCrossings, rootList) <- flip execStateT (0, listArray (1, halfN) $ repeat [], []) $
		let	lonerSymmetry = maximumSubGroup 4
			loner = makeCrossing' $ fromTangle lonerProjection lonerSymmetry NonDirectSumDecomposable 0
		in flip fix (lonerTangle loner, lonerSymmetry) $ \ growTree (rootTemplate, rootSymmetry) -> do
			(!free, !prevCrossings, !prevList) <- get
			let rootCrossing = buildCrossingType rootTemplate rootSymmetry free
			let rootN = numberOfCrossings $ subTangle rootCrossing
			let crossings = prevCrossings // [(rootN, rootCrossing : (prevCrossings ! rootN))]
			let root = lonerTangle $ makeCrossing' rootCrossing
			put $! (free + 1, crossings, ((root, rootSymmetry), crossings) : prevList)

			let glueTemplates curN ancestor =
				forM_ [1 .. halfN - curN] $ \ cn ->
					forM_ (templateDescendants (crossings ! cn) ancestor) $ \ (child, childSymmetry) -> do
						let t = curN + cn - halfN + (numberOfLegs child `div` 2) - 2
						when (t <= 0) $
							case numberOfLegs child of
								4 -> growTree (child, childSymmetry)
								_ -> lift (yield child childSymmetry) >> glueTemplates (curN + cn) (child, childSymmetry)

			let glueDirectSums curN ancestor =
				forM_ [1 .. halfN - curN] $ \ cn ->
					forM_ (directSumDescendants (crossings ! cn) ancestor) $ \ (child, childSymmetry) -> do
						let t = curN + cn - halfN + (numberOfLegs child `div` 2) - 2
						when (t <= 0) $ do
							growTree (child, childSymmetry)
							glueDirectSums (curN + cn) (child, childSymmetry)

			lift $ yield rootTemplate rootSymmetry
			glueTemplates rootN (root, rootSymmetry)
			glueDirectSums rootN (root, rootSymmetry)

	flip evalStateT finalFree $ forM_ rootList $ fix $ \ grow (root, crossings) -> do
		let tree (rootTemplate, rootSymmetry) = do
			free <- get
			put $! free + 1
			lift $ yield rootTemplate rootSymmetry
			grow ((lonerTangle $ makeCrossing' $ buildCrossingType rootTemplate rootSymmetry free, rootSymmetry), finalCrossings)

		let glueTemplates curN ancestor =
			forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
				forM_ (templateDescendants (crossings ! cn) ancestor) $ \ (child, childSymmetry) -> do
					let t = curN + cn - halfN + (numberOfLegs child `div` 2) - 2
					unless (triangle && t > maxN - halfN) $
						case numberOfLegs child of
							4 -> when (t > 0) $ tree (child, childSymmetry)
							_ -> do
								when (t > 0) $ lift $ yield child childSymmetry
								glueTemplates (curN + cn) (child, childSymmetry)

		let glueDirectSums curN ancestor =
			forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
				forM_ (directSumDescendants (crossings ! cn) ancestor) $ \ (child, childSymmetry) -> do
					let t = curN + cn - halfN + (numberOfLegs child `div` 2) - 2
					unless (triangle && t > maxN - halfN) $ do
						when (t > 0) $ tree (child, childSymmetry)
						glueDirectSums (curN + cn) (child, childSymmetry)

		let rootN = numberOfCrossingsAfterSubstitution $ fst root
		glueTemplates rootN root
		glueDirectSums rootN root


generateFlypeEquivalentDecomposition :: (Monad m) => Int -> (SubTangleTangle ProjectionCrossing -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalentDecomposition = generateFlypeEquivalentDecomposition' False


generateFlypeEquivalentDecompositionInTriangle :: (Monad m) => Int -> (SubTangleTangle ProjectionCrossing -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalentDecompositionInTriangle = generateFlypeEquivalentDecomposition' True


generateFlypeEquivalent :: (Monad m) => Int -> (TangleProjection -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalent maxN yield =
	generateFlypeEquivalentDecomposition maxN
		(\ tangle symmetry -> yield (substituteTangle tangle) symmetry)


generateFlypeEquivalentInTriangle :: (Monad m) => Int -> (TangleProjection -> DnSubGroup -> m ()) -> m ()
generateFlypeEquivalentInTriangle maxN yield =
	generateFlypeEquivalentDecompositionInTriangle maxN
		(\ tangle symmetry -> yield (substituteTangle tangle) symmetry)
