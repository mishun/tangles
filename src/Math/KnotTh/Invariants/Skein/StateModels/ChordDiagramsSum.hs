module Math.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum
    ( ChordDiagramsSum
    ) where

import Data.Function (on)
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.Base ((!), (//), bounds, elems, array, listArray, newArray, newArray_, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when, foldM_)
import Control.DeepSeq
import Control.Parallel.Strategies
import Text.Printf
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move
import Math.KnotTh.Invariants.Skein.Relation


data ChordDiagram a = ChordDiagram !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor ChordDiagram where
    fmap f (ChordDiagram p x) = ChordDiagram p $! f x


instance (NFData a) => NFData (ChordDiagram a) where
    rnf (ChordDiagram p x) = p `seq` rnf x


instance (Show a) => Show (ChordDiagram a) where
    show (ChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data ChordDiagramsSum a = ChordDiagramsSum !Int ![ChordDiagram a] deriving (Eq, Ord)


instance Functor ChordDiagramsSum where
    fmap f (ChordDiagramsSum order list) = ChordDiagramsSum order $ map (fmap f) list


instance (NFData a) => NFData (ChordDiagramsSum a) where
    rnf (ChordDiagramsSum _ list) = rnf list


instance (Show a) => Show (ChordDiagramsSum a) where
    show (ChordDiagramsSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: ChordDiagram a -> ChordDiagramsSum a
singletonStateSum summand @ (ChordDiagram a _) =
    ChordDiagramsSum (1 + snd (bounds a)) [summand]


concatStateSums :: (Eq a, Num a) => [ChordDiagramsSum a] -> ChordDiagramsSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (ChordDiagramsSum order _ : _) =
    let s = map (\ (!k, !v) -> ChordDiagram k v) $
            filter ((/= 0) . snd) $! M.toList $
                foldl' (\ !m (ChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                    concatMap (\ (ChordDiagramsSum order' list') ->
                            if order' == order
                                then list'
                                else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                        ) list
    in ChordDiagramsSum order (s `using` evalList rseq)


mapStateSum :: (Eq a, Num a) => (ChordDiagram a -> ChordDiagramsSum a) -> ChordDiagramsSum a -> ChordDiagramsSum a
mapStateSum _ (ChordDiagramsSum order []) = ChordDiagramsSum order []
mapStateSum f (ChordDiagramsSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => ChordDiagramsSum a -> (ChordDiagram a -> m ()) -> m ()
forAllSummands (ChordDiagramsSum _ list) = forM_ list


canonicalHeightOrder :: Int -> (Int, Int) -> (Int, Int) -> Ordering
canonicalHeightOrder _ (!a', !b') (!c', !d') =
    let a = min a' b' ; b = max a' b'
        c = min c' d' ; d = max c' d'
    in case () of
        _ | a < c && b > c && b < d -> GT
          | a > c && a < d && b > d -> LT
          | otherwise               -> EQ


restoreBasicTangle :: UArray Int Int -> NonAlternatingTangle
restoreBasicTangle !chordDiagram =
    let (0, l) = bounds chordDiagram

        restore :: UArray Int Int -> UArray Int Int -> [Int] -> NonAlternatingTangle
        restore a _ [] = implode (0, map ((,) 0) $ elems a, [])
        restore a h (i : rest) =
            case canonicalHeightOrder (l + 1) (i, i') (j, j') of
                EQ -> restore a h rest
                _  -> let tangle = restore (a // [(i, j'), (j, i'), (i', j), (j', i)]) (h // [(i, h ! j), (j, h ! i)]) [0 .. l]
                      in rotateTangle i $ crossingTangle $ glueToBorder (nthLeg tangle j) 2 $
                          if h ! i < h ! j
                              then overCrossing
                              else underCrossing
            where
                i' = a ! i
                j = (i + 1) `mod` (l + 1)
                j' = a ! j

    in restore chordDiagram (listArray (0, l) $ map (\ i -> min i $ chordDiagram ! i) [0 .. l]) [0 .. l]


decomposeTangle :: (SkeinRelation r a) => r -> Int -> a -> NonAlternatingTangle -> ChordDiagramsSum a
decomposeTangle relation !depth !factor !tangle
    | numberOfFreeLoops tangle > 0  =
        decomposeTangle relation depth
            (factor * (circleFactor relation ^ numberOfFreeLoops tangle))
            (changeNumberOfFreeLoops tangle 0)
    | otherwise                     =
        let (n, marks, threads) = allThreadsWithMarks tangle

            expectedPassOver =
                let threadIndex :: UArray Int Int
                    threadIndex = array (1, n) $ flip map threads $ \ (i, thread) ->
                        case thread of
                            []                     -> error "internal error"
                            (h, _) : _ | isLeg h   -> (i, on min legPlace (fst $ head thread) (snd $ last thread))
                                       | otherwise -> (i, numberOfLegs tangle + i)

                    order :: UArray Int Int
                    order = array (dartIndexRange tangle) $ do
                        (_, thread) <- threads
                        (i, (a, b)) <- zip [0 ..] thread
                        [(dartIndex a, 2 * i), (dartIndex b, 2 * i + 1)]

                in \ d0 -> on (<) ((\ d -> (threadIndex ! abs (marks ! d), order ! d)) . dartIndex) d0 (nextCCW d0)

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
                in singletonStateSum $ ChordDiagram a $ factor *
                    ((if w >= 0 then twistPFactor else twistNFactor) relation ^ abs w) *
                        (circleFactor relation ^ (n - numberOfLegs tangle `div` 2))

            tryCrossing (c : rest) =
                let [d0, d1, d2, d3] = incidentDarts c
                in if passOver d0 == expectedPassOver d0
                    then tryCrossing rest
                    else concatStateSums
                        [ decomposeTangle relation (depth + 1) (factor * smoothLplusFactor relation) $ move tangle $
                            modifyC False invertCrossing [c]

                        , decomposeTangle relation (depth + 1) (factor * (if isOverCrossing (crossingState c) then smoothLzeroFactor else smoothLinftyFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 2
                                      | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 1
                                      | opposite d0 == d3                      -> connectC [(opposite d1, opposite d2)]
                                      | opposite d1 == d2                      -> connectC [(opposite d0, opposite d3)]
                                      | otherwise                              -> substituteC [(opposite d0, d1), (opposite d3, d2)]
                                maskC [c]

                        , decomposeTangle relation (depth + 1) (factor * (if isOverCrossing (crossingState c) then smoothLinftyFactor else smoothLzeroFactor) relation) $
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


bruteForceRotate :: (SkeinRelation r a) => r -> Int -> ChordDiagramsSum a -> ChordDiagramsSum a
bruteForceRotate relation rot
    | rot == 0   = id
    | otherwise  = mapStateSum (\ (ChordDiagram a factor) -> decomposeTangle relation 0 factor $ rotateTangle rot $ restoreBasicTangle a)


bruteForceMirror :: (SkeinRelation r a) => r -> ChordDiagramsSum a -> ChordDiagramsSum a
bruteForceMirror relation =
    mapStateSum (\ (ChordDiagram a factor) -> decomposeTangle relation 0 factor $ mirrorTangle $ restoreBasicTangle a)


instance StateModel ChordDiagramsSum where
    complexityRank (ChordDiagramsSum _ list) = length list

    initialize _ =
        concatStateSums . map (\ (skein, factor) ->
                let a = listArray (0, 3) $
                        case skein of
                            Lplus  -> [2, 3, 0, 1]
                            Lzero  -> [3, 2, 1, 0]
                            Linfty -> [1, 0, 3, 2]
                in singletonStateSum $ ChordDiagram a factor
            )

    asConst _ (ChordDiagramsSum _ []) = 0
    asConst _ (ChordDiagramsSum _ [ChordDiagram _ x]) = x
    asConst _ _ = error "takeAsConst: constant expected"

    glueHandle relation !p !preSum @ (ChordDiagramsSum !degree _) =
        let !p' = (p + 1) `mod` degree

            !subst = runSTUArray $ do
                a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
                foldM_ (\ !k !i ->
                    if i == p' || i == p
                        then return $! k
                        else writeArray a i k >> (return $! k + 1)
                    ) 0 $ [p .. degree - 1] ++ [0 .. p - 1]
                return $! a

            !postSum = flip mapStateSum preSum $ \ (ChordDiagram x k) ->
                let t = restoreBasicTangle x
                in decomposeTangle relation 0 k $ glueTangles 2 (nthLeg t p) (firstLeg identityTangle)
        in (subst, postSum)

    connect relation (!p, !sumV @ (ChordDiagramsSum !degreeV _)) (!q, !sumU @ (ChordDiagramsSum !degreeU _)) =
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

            !result = flip mapStateSum sumV $ \ (ChordDiagram xa ka) ->
                let ta = restoreBasicTangle xa
                in flip mapStateSum sumU $ \ (ChordDiagram xb kb) ->
                    let tb = restoreBasicTangle xb
                    in decomposeTangle relation 0 (ka * kb) $ glueTangles 1 (nthLeg ta p) (nthLeg tb q)
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
                readSTRef result >>= \ !list ->
                    writeSTRef result $! ChordDiagram x factor : list

            substState factor (v : rest) = do
                r <- readArray rot v
                forAllSummands (bruteForceRotate relation (-r) $ internals ! v) $ \ (ChordDiagram x f) -> do
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
