module Math.Topology.KnotTh.Invariants.Skein.StateModels.OrientedChordDiagramsSum
    ( OrientedChordDiagramsSum
    ) where

import Data.Function (on)
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.IArray ((!), (//), bounds, elems, array, listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when, foldM_)
import Control.DeepSeq
import Text.Printf
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.Moves.Move
import Math.Topology.KnotTh.Invariants.Skein.Relation


data OrientedChordDiagram a = OrientedChordDiagram !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor OrientedChordDiagram where
    fmap f (OrientedChordDiagram p x) = OrientedChordDiagram p $! f x


instance (NFData a) => NFData (OrientedChordDiagram a) where
    rnf (OrientedChordDiagram p x) = p `seq` rnf x


instance (Show a) => Show (OrientedChordDiagram a) where
    show (OrientedChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data OrientedChordDiagramsSum a = OrientedChordDiagramsSum !Int ![OrientedChordDiagram a] deriving (Eq, Ord)


instance Functor OrientedChordDiagramsSum where
    fmap f (OrientedChordDiagramsSum order list) = OrientedChordDiagramsSum order $ map (fmap f) list


instance (NFData a) => NFData (OrientedChordDiagramsSum a) where
    rnf (OrientedChordDiagramsSum _ list) = rnf list


instance (Show a) => Show (OrientedChordDiagramsSum a) where
    show (OrientedChordDiagramsSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: OrientedChordDiagram a -> OrientedChordDiagramsSum a
singletonStateSum summand @ (OrientedChordDiagram a _) =
    OrientedChordDiagramsSum (1 + snd (bounds a)) [summand]


concatStateSums :: (Eq a, Num a) => [OrientedChordDiagramsSum a] -> OrientedChordDiagramsSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (OrientedChordDiagramsSum order _ : _) =
    OrientedChordDiagramsSum order $ map (\ (!k, !v) -> OrientedChordDiagram k v) $
        filter ((/= 0) . snd) $ M.toList $
            foldl' (\ !m (OrientedChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                concatMap (\ (OrientedChordDiagramsSum order' list') ->
                        if order' == order
                            then list'
                            else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                    ) list


mapStateSum :: (Eq a, Num a) => (OrientedChordDiagram a -> OrientedChordDiagramsSum a) -> OrientedChordDiagramsSum a -> OrientedChordDiagramsSum a
mapStateSum _ (OrientedChordDiagramsSum order []) = OrientedChordDiagramsSum order []
mapStateSum f (OrientedChordDiagramsSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => OrientedChordDiagramsSum a -> (OrientedChordDiagram a -> m ()) -> m ()
forAllSummands (OrientedChordDiagramsSum _ list) = forM_ list


restoreBasicTangle :: UArray Int Int -> NATangle
restoreBasicTangle chordDiagram =
    let (0, l) = bounds chordDiagram

        restore :: UArray Int Int -> UArray Int Int -> [Int] -> NATangle
        restore a _ [] = implode (0, map ((,) 0) $ elems a, [])
        restore !a !h ((!i) : rest)
            | not cross  = restore a h rest
            | otherwise  =
                let tangle = restore (a // [(i, j'), (j, i'), (i', j), (j', i)]) (h // [(i, h ! j), (j, h ! i)]) [0 .. l]
                in rotateTangle i $ crossingTangle $ glueToBorder (nthLeg tangle j) 2 $
                    if h ! i < h ! j
                        then overCrossing
                        else underCrossing
            where
                i' = a ! i
                j = (i + 1) `mod` (l + 1)
                j' = a ! j
                cross = (min i i' < min j j' && max i i' < max j j' && min j j' < max i i')
                    || (min j j' < min i i' && max j j' < max i i' && min i i' < max j j')

    in restore chordDiagram (listArray (0, l) $ map (\ i -> min i $ chordDiagram ! i) [0 .. l]) [0 .. l]


decomposeTangle :: (SkeinRelation r a) => r -> a -> NATangle -> OrientedChordDiagramsSum a
decomposeTangle relation !factor !tangle
    | numberOfFreeLoops tangle > 0  =
        decomposeTangle relation
            (factor * (circleFactor relation ^ numberOfFreeLoops tangle))
            (changeNumberOfFreeLoops tangle 0)
    | otherwise                     =
        let (n, marks, threads) = allThreadsWithMarks tangle

            threadIndex :: UArray Int Int
            threadIndex = array (1, n) $ flip map threads $ \ (i, thread) ->
                case thread of
                    []                     -> error "internal error"
                    (h, _) : _ | isLeg h   -> (i, on min legPlace (fst $ head thread) (snd $ last thread))
                               | otherwise -> (i, numberOfLegs tangle + i)

            order :: UArray NATangleDart Int
            order = array (dartsRange tangle) $ do
                (_, thread) <- threads
                (i, (a, b)) <- zip [0 ..] thread
                [(a, 2 * i), (b, 2 * i + 1)]

            tryCrossing [] =
                let a = array (0, numberOfLegs tangle - 1) $ do
                        (_, thread) <- threads
                        case thread of
                            (h, _) : _ | isLeg h ->
                                let i = legPlace $ fst $ head thread
                                    j = legPlace $ snd $ last thread
                                in [(i, j), (j, i)]
                            _                    -> []

                    w = selfWrithe tangle
                in singletonStateSum $ OrientedChordDiagram a $ factor *
                    ((if w >= 0 then twistPFactor else twistNFactor) relation ^ abs w) *
                        (circleFactor relation ^ (n - numberOfLegs tangle `div` 2))

            tryCrossing (c : rest) =
                let [d0, d1, d2, d3] = incidentDarts c
                in if passOver d0 == on (<) (\ d -> (threadIndex ! abs (marks ! d), order ! d)) d0 d1
                    then tryCrossing rest
                    else concatStateSums
                        [ decomposeTangle relation (factor * smoothLplusFactor relation) $ move tangle $
                            modifyC False invertCrossing [c]

                        , decomposeTangle relation (factor * (if isOverCrossing (crossingState c) then smoothLzeroFactor else smoothLinftyFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 2
                                      | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 1
                                      | opposite d0 == d3                      -> connectC [(opposite d1, opposite d2)]
                                      | opposite d1 == d2                      -> connectC [(opposite d0, opposite d3)]
                                      | otherwise                              -> substituteC [(opposite d0, d1), (opposite d3, d2)]
                                maskC [c]

                        , decomposeTangle relation (factor * (if isOverCrossing (crossingState c) then smoothLinftyFactor else smoothLzeroFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 2
                                      | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 1
                                      | opposite d0 == d1                      -> connectC [(opposite d2, opposite d3)]
                                      | opposite d3 == d2                      -> connectC [(opposite d0, opposite d1)]
                                      | otherwise                              -> substituteC [(opposite d0, d3), (opposite d1, d2)]
                                maskC [c]
                        ]

        in tryCrossing $ allCrossings tangle


bruteForceRotate :: (SkeinRelation r a) => r -> Int -> OrientedChordDiagramsSum a -> OrientedChordDiagramsSum a
bruteForceRotate relation rot
    | rot == 0   = id
    | otherwise  = mapStateSum (\ (OrientedChordDiagram a factor) -> decomposeTangle relation factor $ rotateTangle rot $ restoreBasicTangle a)


bruteForceMirror :: (SkeinRelation r a) => r -> OrientedChordDiagramsSum a -> OrientedChordDiagramsSum a
bruteForceMirror relation =
    mapStateSum (\ (OrientedChordDiagram a factor) -> decomposeTangle relation factor $ mirrorTangle $ restoreBasicTangle a)


instance StateModel OrientedChordDiagramsSum where
    complexityRank (OrientedChordDiagramsSum _ list) = length list

    projection (OrientedChordDiagramsSum _ list) = do
        OrientedChordDiagram cd x <- list
        return (restoreBasicTangle cd, x)

    initialize _ =
        concatStateSums . map (\ (skein, factor) ->
                let a = listArray (0, 3) $
                        case skein of
                            Lplus  -> [2, 3, 0, 1]
                            Lzero  -> [3, 2, 1, 0]
                            Linfty -> [1, 0, 3, 2]
                in singletonStateSum $ OrientedChordDiagram a factor
            )

    asConst _ (OrientedChordDiagramsSum _ []) = 0
    asConst _ (OrientedChordDiagramsSum _ [OrientedChordDiagram _ x]) = x
    asConst _ _ = error "takeAsConst: constant expected"

    glueHandle relation !p preSum @ (OrientedChordDiagramsSum !degree _) =
        let !p' = (p + 1) `mod` degree

            !subst = runSTUArray $ do
                a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
                foldM_ (\ !k !i ->
                    if i == p' || i == p
                        then return $! k
                        else writeArray a i k >> (return $! k + 1)
                    ) 0 $ [p .. degree - 1] ++ [0 .. p - 1]
                return $! a

            !postSum = flip mapStateSum preSum $ \ (OrientedChordDiagram x k) ->
                let t = restoreBasicTangle x
                in decomposeTangle relation k $ glueTangles 2 (nthLeg t p) (firstLeg identityTangle)
        in (subst, postSum)

    connect relation (!p, sumV @ (OrientedChordDiagramsSum !degreeV _)) (!q, sumU @ (OrientedChordDiagramsSum !degreeU _)) =
        let !substV = runSTUArray $ do
                a <- newArray (0, degreeV - 1) (-1) :: ST s (STUArray s Int Int)
                forM_ [p + 1 .. degreeV - 1] $ \ !i ->
                    writeArray a i $ i - p - 1
                forM_ [0 .. p - 1] $ \ !i ->
                    writeArray a i $ i + degreeV - p - 1
                return $! a

            !substU = runSTUArray $ do
                a <- newArray (0, degreeU - 1) (-1) :: ST s (STUArray s Int Int)
                forM_ [q + 1 .. degreeU - 1] $ \ !i ->
                    writeArray a i $ i + (degreeV - q - 2)
                forM_ [0 .. q - 1] $ \ !i ->
                    writeArray a i $ i + (degreeV + degreeU - q - 2)
                return $! a

            !result = flip mapStateSum sumV $ \ (OrientedChordDiagram xa ka) ->
                let ta = restoreBasicTangle xa
                in flip mapStateSum sumU $ \ (OrientedChordDiagram xb kb) ->
                    let tb = restoreBasicTangle xb
                    in decomposeTangle relation (ka * kb) $ glueTangles 1 (nthLeg ta p) (nthLeg tb q)
        in (substV, substU, result)

    assemble relation border connections internals global = runST $ do
        let (0, l) = bounds border
        let (1, n) = bounds internals

        cd <- newArray_ (0, l) :: ST s (STUArray s Int Int)
        rot <- newArray (1, n) (-1) :: ST s (STUArray s Int Int)

        forM_ [0 .. l] $ \ !i -> do
            let (v, p) = border ! i
            if v == 0
                then writeArray cd i p
                else do
                    r <- readArray rot v
                    when (r < 0) $ writeArray rot v p

        result <- newSTRef []
        let substState factor [] = do
                x <- freeze cd
                modifySTRef' result (OrientedChordDiagram x factor :)

            substState factor (v : rest) = do
                r <- readArray rot v
                forAllSummands (bruteForceRotate relation (-r) $ internals ! v) $ \ (OrientedChordDiagram x f) -> do
                    let (0, k) = bounds x
                    forM_ [0 .. k] $ \ !i -> do
                        let a = (connections ! v) ! ((i + r) `mod` (k + 1))
                        let b = (connections ! v) ! (((x ! i) + r) `mod` (k + 1))
                        writeArray cd a b
                    substState (factor * f) rest

        substState global [1 .. n]
        (concatStateSums . map singletonStateSum) `fmap` readSTRef result

    rotate = bruteForceRotate

    mirror = bruteForceMirror
