module Math.Topology.KnotTh.Tabulation.TangleFlypeClasses
    ( generateFlypeEquivalentDecomposition
    , generateFlypeEquivalentDecompositionInTriangle
    , generateFlypeEquivalent
    , generateFlypeEquivalentInTriangle
    ) where

import Control.Arrow (first)
import Control.Monad (forM_, when, guard)
import Control.Monad.State.Strict (evalStateT, execStateT, get, put, lift)
import Data.Function (fix, on)
import Data.List (nubBy)
import Data.Maybe (maybeToList)
import qualified Data.Vector as V
import Math.Topology.KnotTh.Algebra.Dihedral.Dn
import Math.Topology.KnotTh.Algebra.Dihedral.D4 (fromDnSubGroup, subGroupD4)
import Math.Topology.KnotTh.Knotted.Crossings.SubTangle
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Tabulation.TangleFlypeClasses.Flypes


templateDescendants :: Bool -> Int -> [SubTangleCrossing ProjectionCrossing]
                        -> Int -> Int -> (SubTangleTangle ProjectionCrossing, SubGroup Dn)
                            -> [(SubTangleTangle ProjectionCrossing, SubGroup Dn)]
templateDescendants tri maxN crossings cn curN (tangle, symmetry) = do
    let l = numberOfLegs tangle
    guard $ numberOfVertices tangle == 1 || l > 4
    gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
    guard $ not tri || (diagonalIndex (curN + cn) (nextNumberOfLegs l gl) <= maxN)
    (leg, inducedSymmetry) <- uniqueGlueSites' gl (tangle, symmetry)
    guard $ testNoMultiEdges (nthLeg tangle leg) gl
    cr <- crossings
    st <- possibleSubTangleOrientations cr inducedSymmetry
    maybeToList $ do
        let root = glueToBorder gl (tangle, leg) st
        (sym, _, _) <- rootingSymmetryTest root
        guard $ gl < 3 || testFlow4 root
        return (vertexOwner root, sym)


directSumDescendants :: [SubTangleCrossing ProjectionCrossing]
                        -> (Tangle4 (SubTangleCrossing ProjectionCrossing), SubGroup Dn)
                            -> [(Tangle4 (SubTangleCrossing ProjectionCrossing), SubGroup Dn)]
directSumDescendants crossings (t4, symmetry) = do
    let preTest t cr legIndex =
            let leg = nthLeg t legIndex
                lp = directSumDecompositionType cr
                rp = directSumDecompositionTypeInVertex (opposite leg)
            in if | numberOfLegs t /= 4     -> False
                  | testNoMultiEdges leg 2  -> False
                  | lp == DirectSum01x23    -> False
                  | numberOfVertices t == 1 -> rp /= DirectSum12x30
                  | otherwise               -> True

        postTest root (t, legIndex) s =
            let leg = nthLeg t legIndex
                coLeg = nextCCW $ nextCCW leg
                flypeS =
                    case additionalFlypeSymmetry (tangle' $ vertexOwner root) of
                        Just x  -> addSymmetryToSubGroup s x
                        Nothing -> s
            in case (isLonerInVertex root, isLonerInVertex $ endVertex leg, isLonerInVertex $ endVertex coLeg) of
                (False, False, False) -> return $! s
                (True , True , _    ) -> return $! flypeS
                (True , False, False) ->
                    let (_, leftCCW) = rootCodeLeg leg ccw
                        (_, leftCW ) = rootCodeLeg (nextCW leg) cw
                        (_, rightCCW) = rootCodeLeg coLeg ccw
                        (_, rightCW ) = rootCodeLeg (nextCW coLeg) cw
                    in if min leftCCW leftCW <= min rightCCW rightCW
                        then return $! flypeS
                        else Nothing
                _                     -> Nothing

    cr <- crossings
    let tangle = toTangle t4
    if isLonerInSub cr
        then map snd $ nubBy (on (==) fst) $ do
            leg <- [0 .. 3]
            st <- possibleSubTangleOrientations cr Nothing
            guard $ preTest tangle st leg
            maybeToList $ do
                let root = glueToBorder 2 (tangle, leg) st
                (s, _, rc) <- rootingSymmetryTest root
                sym <- postTest root (tangle, leg) s
                return (rc, (tangle' $ vertexOwner root, sym))
        else do
            (leg, inducedSymmetry) <- uniqueGlueSites' 2 (tangle, symmetry)
            st <- possibleSubTangleOrientations cr inducedSymmetry
            guard $ preTest tangle st leg
            maybeToList $ do
                let root = glueToBorder 2 (tangle, leg) st
                (s, _, _) <- rootingSymmetryTest root
                sym <- postTest root (tangle, leg) s
                return (tangle' $ vertexOwner root, sym)


generateFlypeEquivalentDecomposition' :: (Monad m) => Bool -> Int -> ((SubTangleTangle ProjectionCrossing, SubGroup Dn) -> m ()) -> m ()
generateFlypeEquivalentDecomposition' triangle maxN yield = do
    let buildCrossingType template symmetry =
            let sumType | (a == b) && (c == d) && (a /= c)  = DirectSum01x23
                        | (b == c) && (a == d) && (a /= b)  = DirectSum12x30
                        | otherwise                         = NonDirectSumDecomposable
                    where
                        [a, b, c, d] = map endVertex $ allLegs $ toTangle template
            in makeSubTangleCrossing' template symmetry sumType

    let halfN = maxN `div` 2

    (finalFree, finalCrossings, rootList) <-
        flip execStateT (0, V.replicate (halfN + 1) [], []) $
            let lonerSymmetry = maximumSubGroup 4
                loner = makeSubTangleCrossing lonerProjection subGroupD4 NonDirectSumDecomposable 0
            in flip fix (lonerTangle loner, lonerSymmetry) $ \ growTree (rootTemplate, rootSymmetry) -> do
                (!free, !prevCrossings, !prevList) <- get
                let rootCrossing = buildCrossingType rootTemplate (fromDnSubGroup rootSymmetry) free
                let rootN = numberOfVerticesInSub rootCrossing
                let crossings = prevCrossings V.// [(rootN, rootCrossing : (prevCrossings V.! rootN))]
                let root = lonerTangle rootCrossing
                put (free + 1, crossings, ((root, rootSymmetry), crossings) : prevList)

                let glueTemplates curN ancestor =
                        forM_ [1 .. halfN - curN] $ \ cn ->
                            forM_ (templateDescendants True halfN (crossings V.! cn) cn curN ancestor) $ \ child@(childTangle, _) ->
                                case numberOfLegs childTangle of
                                    4 -> growTree $ first tangle' child
                                    _ -> glueTemplates (curN + cn) child

                let glueDirectSums curN ancestor =
                        forM_ [1 .. halfN - curN] $ \ cn ->
                            forM_ (directSumDescendants (crossings V.! cn) ancestor) $ \ child -> do
                                growTree child
                                glueDirectSums (curN + cn) child

                lift $ yield (toTangle rootTemplate, rootSymmetry)
                glueTemplates rootN (toTangle root, rootSymmetry)
                glueDirectSums rootN (root, rootSymmetry)

    flip evalStateT finalFree $ forM_ rootList $ fix $ \ grow (root, crossings) -> do
        let tree curN (rootTemplate, rootSymmetry) =
                when (curN > halfN) $ do
                    free <- get
                    put $! free + 1
                    lift $ yield (toTangle rootTemplate, rootSymmetry)
                    grow ((lonerTangle $ buildCrossingType rootTemplate (fromDnSubGroup rootSymmetry) free, rootSymmetry), finalCrossings)

        let glueTemplates curN ancestor =
                forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
                    forM_ (templateDescendants triangle maxN (crossings V.! cn) cn curN ancestor) $ \ child@(childTangle, childSymmetry) ->
                        case numberOfLegs childTangle of
                            4 -> tree (curN + cn) $ first tangle' child
                            _ -> lift (yield (childTangle, childSymmetry)) >> glueTemplates (curN + cn) child

        let glueDirectSums curN ancestor =
                forM_ [1 .. min halfN (maxN - curN)] $ \ cn ->
                    forM_ (directSumDescendants (crossings V.! cn) ancestor) $ \ child -> do
                        tree (curN + cn) child
                        glueDirectSums (curN + cn) child

        let rootN = sum $ map numberOfVerticesInVertex $ allVertices $ fst root --numberOfVerticesAfterSubstitution $ toTangle $ fst root
        glueTemplates rootN $ first toTangle root
        glueDirectSums rootN root


generateFlypeEquivalentDecomposition :: (Monad m) => Int -> ((SubTangleTangle ProjectionCrossing, SubGroup Dn) -> m ()) -> m ()
generateFlypeEquivalentDecomposition =
    generateFlypeEquivalentDecomposition' False


generateFlypeEquivalentDecompositionInTriangle :: (Monad m) => Int -> ((SubTangleTangle ProjectionCrossing, SubGroup Dn) -> m ()) -> m ()
generateFlypeEquivalentDecompositionInTriangle =
    generateFlypeEquivalentDecomposition' True


generateFlypeEquivalent :: (Monad m) => Int -> ((TangleProjection, SubGroup Dn) -> m ()) -> m ()
generateFlypeEquivalent maxN =
    generateFlypeEquivalentDecomposition maxN . (. first substituteSubTangles)


generateFlypeEquivalentInTriangle :: (Monad m) => Int -> ((TangleProjection, SubGroup Dn) -> m ()) -> m ()
generateFlypeEquivalentInTriangle maxN =
    generateFlypeEquivalentDecompositionInTriangle maxN . (. first substituteSubTangles)
