module Math.Topology.KnotTh.Tabulation.TangleFlypeClasses
    ( generateFlypeEquivalentDecomposition
    , generateFlypeEquivalentDecompositionInTriangle
    , generateFlypeEquivalent
    , generateFlypeEquivalentInTriangle
    ) where

import Data.Function (fix, on)
import Data.Maybe (maybeToList)
import Data.List (nubBy)
import Data.Array ((!), (//), listArray)
import Control.Monad.State.Strict (evalStateT, execStateT, get, put, lift)
import Control.Arrow (first)
import Control.Monad (forM_, when, guard)
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Knotted.Crossings.SubTangle
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Tabulation.TangleFlypeClasses.Flypes


templateDescendants
    :: Bool -> Int -> [SubTangleCrossing ProjectionCrossing]
        -> Int -> Int -> (SubTangleTangle ProjectionCrossing, Dn.DnSubGroup)
            -> [(SubTangleTangle ProjectionCrossing, Dn.DnSubGroup)]

templateDescendants tri maxN crossings cn curN (tangle, symmetry) = do
    let l = numberOfLegs tangle
    guard $ numberOfVertices tangle == 1 || l > 4
    gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
    guard $ not tri || (diagonalIndex (curN + cn) (nextNumberOfLegs l gl) <= maxN)
    (leg, inducedSymmetry) <- uniqueGlueSites' gl (tangle, symmetry)
    guard $ testNoMultiEdges leg gl
    cr <- crossings
    st <- possibleSubTangleOrientations cr inducedSymmetry
    maybeToList $ do
        let root = glueToBorder leg gl st
        (sym, _, _) <- rootingSymmetryTest root
        guard $ gl < 3 || testFlow4 root
        return (vertexOwner root, sym)


directSumDescendants
    :: [SubTangleCrossing ProjectionCrossing]
        -> (SubTangleTangle ProjectionCrossing, Dn.DnSubGroup)
            -> [(SubTangleTangle ProjectionCrossing, Dn.DnSubGroup)]

directSumDescendants crossings (tangle, symmetry) = do
    let preTest cr leg =
            let t = dartOwner leg
                lp = directSumDecompositionTypeOfCrossing cr
                rp = directSumDecompositionTypeInVertex (opposite leg)
            in if | numberOfLegs t /= 4     -> False
                  | testNoMultiEdges leg 2  -> False
                  | lp == DirectSum01x23    -> False
                  | numberOfVertices t == 1 -> rp /= DirectSum12x30
                  | otherwise               -> True

        postTest root leg s =
            let coLeg = nextCCW $ nextCCW leg
                flypeS =
                    case additionalFlypeSymmetry (vertexOwner root) of
                        Just x  -> Dn.addSymmetryToSubGroup s x
                        Nothing -> s
            in case (isLonerInVertex root, isLonerInVertex $ endVertex leg, isLonerInVertex $ endVertex coLeg) of
                (False, False, False) -> return $! s
                (True , True , _    ) -> return $! flypeS
                (True , False, False) ->
                    let (_, leftCCW) = rootCodeLeg leg R.ccw
                        (_, leftCW ) = rootCodeLeg (nextCW leg) R.cw
                        (_, rightCCW) = rootCodeLeg coLeg R.ccw
                        (_, rightCW ) = rootCodeLeg (nextCW coLeg) R.cw
                    in if min leftCCW leftCW <= min rightCCW rightCW
                        then return $! flypeS
                        else Nothing
                _                     -> Nothing

    cr <- crossings
    if isLonerCrossing cr
        then map snd $ nubBy (on (==) fst) $ do
            leg <- allLegs tangle
            st <- possibleSubTangleOrientations cr Nothing
            guard $ preTest st leg
            maybeToList $ do
                let root = glueToBorder leg 2 st
                (s, _, rc) <- rootingSymmetryTest root
                sym <- postTest root leg s
                return (rc, (vertexOwner root, sym))
        else do
            (leg, inducedSymmetry) <- uniqueGlueSites' 2 (tangle, symmetry)
            st <- possibleSubTangleOrientations cr inducedSymmetry
            guard $ preTest st leg
            maybeToList $ do
                let root = glueToBorder leg 2 st
                (s, _, _) <- rootingSymmetryTest root
                sym <- postTest root leg s
                return (vertexOwner root, sym)


generateFlypeEquivalentDecomposition' :: (Monad m) => Bool -> Int -> ((SubTangleTangle ProjectionCrossing, Dn.DnSubGroup) -> m ()) -> m ()
generateFlypeEquivalentDecomposition' triangle maxN yield = do
    let buildCrossingType template symmetry =
            let sumType
                    | (a == b) && (c == d) && (a /= c)  = DirectSum01x23
                    | (b == c) && (a == d) && (a /= b)  = DirectSum12x30
                    | otherwise                         = NonDirectSumDecomposable
                    where
                        [a, b, c, d] = map endVertex $ allLegs template
            in makeSubTangle' template symmetry sumType

    let halfN = maxN `div` 2

    (finalFree, finalCrossings, rootList) <-
        flip execStateT (0, listArray (1, halfN) $ repeat [], []) $
            let lonerSymmetry = Dn.maximumSubGroup 4
                loner = makeSubTangle lonerProjection lonerSymmetry NonDirectSumDecomposable 0
            in flip fix (lonerTangle loner, lonerSymmetry) $ \ growTree (rootTemplate, rootSymmetry) -> do
                (!free, !prevCrossings, !prevList) <- get
                let rootCrossing = buildCrossingType rootTemplate rootSymmetry free
                let rootN = numberOfCrossingVertices rootCrossing
                let crossings = prevCrossings // [(rootN, rootCrossing : (prevCrossings ! rootN))]
                let root = lonerTangle rootCrossing
                put (free + 1, crossings, ((root, rootSymmetry), crossings) : prevList)

                let glueTemplates curN ancestor =
                        forM_ [1 .. halfN - curN] $ \ cn ->
                            forM_ (templateDescendants True halfN (crossings ! cn) cn curN ancestor) $ \ child@(childTangle, _) ->
                                case numberOfLegs childTangle of
                                    4 -> growTree child
                                    _ -> glueTemplates (curN + cn) child

                let glueDirectSums curN ancestor =
                        forM_ [1 .. halfN - curN] $ \ cn ->
                            forM_ (directSumDescendants (crossings ! cn) ancestor) $ \ child -> do
                                growTree child
                                glueDirectSums (curN + cn) child

                lift $ yield (rootTemplate, rootSymmetry)
                glueTemplates rootN (root, rootSymmetry)
                glueDirectSums rootN (root, rootSymmetry)

    flip evalStateT finalFree $ forM_ rootList $ fix $ \ grow (root, crossings) -> do
        let tree curN (rootTemplate, rootSymmetry) =
                when (curN > halfN) $ do
                    free <- get
                    put $! free + 1
                    lift $ yield (rootTemplate, rootSymmetry)
                    grow ((lonerTangle $ buildCrossingType rootTemplate rootSymmetry free, rootSymmetry), finalCrossings)

        let glueTemplates curN ancestor =
                forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
                    forM_ (templateDescendants triangle maxN (crossings ! cn) cn curN ancestor) $ \ child@(childTangle, childSymmetry) ->
                        case numberOfLegs childTangle of
                            4 -> tree (curN + cn) child
                            _ -> lift (yield (childTangle, childSymmetry)) >> glueTemplates (curN + cn) child

        let glueDirectSums curN ancestor =
                forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
                    forM_ (directSumDescendants (crossings ! cn) ancestor) $ \ child -> do
                        tree (curN + cn) child
                        glueDirectSums (curN + cn) child

        let rootN = numberOfVerticesAfterSubstitution $ fst root
        glueTemplates rootN root
        glueDirectSums rootN root


generateFlypeEquivalentDecomposition :: (Monad m) => Int -> ((SubTangleTangle ProjectionCrossing, Dn.DnSubGroup) -> m ()) -> m ()
generateFlypeEquivalentDecomposition =
    generateFlypeEquivalentDecomposition' False


generateFlypeEquivalentDecompositionInTriangle :: (Monad m) => Int -> ((SubTangleTangle ProjectionCrossing, Dn.DnSubGroup) -> m ()) -> m ()
generateFlypeEquivalentDecompositionInTriangle =
    generateFlypeEquivalentDecomposition' True


generateFlypeEquivalent :: (Monad m) => Int -> ((TangleProjection, Dn.DnSubGroup) -> m ()) -> m ()
generateFlypeEquivalent maxN =
    generateFlypeEquivalentDecomposition maxN . (. first substituteTangles)


generateFlypeEquivalentInTriangle :: (Monad m) => Int -> ((TangleProjection, Dn.DnSubGroup) -> m ()) -> m ()
generateFlypeEquivalentInTriangle maxN =
    generateFlypeEquivalentDecompositionInTriangle maxN . (. first substituteTangles)
