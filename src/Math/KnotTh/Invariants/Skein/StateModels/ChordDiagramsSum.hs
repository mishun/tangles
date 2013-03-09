module Math.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum
    ( ChordDiagramsSum
    ) where

import Debug.Trace
import Data.Function (on)
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.Base ((!), (//), bounds, elems, array, listArray, newArray, newArray_, readArray, writeArray, freeze)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Control.DeepSeq
import Control.Parallel.Strategies
import Text.Printf
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move
import Math.KnotTh.Tangle.Moves.ReidemeisterReduction
import Math.KnotTh.Invariants.Skein.Relation


data ChordDiagram a = ChordDiagram {-# UNPACK #-} !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor ChordDiagram where
    fmap f (ChordDiagram p x) = ChordDiagram p $! f x


instance (NFData a) => NFData (ChordDiagram a) where
    rnf (ChordDiagram p x) = p `seq` rnf x


instance (Show a) => Show (ChordDiagram a) where
    show (ChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data ChordDiagramsSum a = ChordDiagramsSum {-# UNPACK #-} !Int ![ChordDiagram a] deriving (Eq, Ord)


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


{-# INLINE haveIntersection #-}
haveIntersection :: (Int, Int) -> (Int, Int) -> Bool
haveIntersection (!a', !b') (!c', !d') =
    let a = min a' b' ; b = max a' b'
        c = min c' d' ; d = max c' d'
    in (a < c && b > c && b < d) || (a > c && a < d && b > d)


{-# INLINE canonicalOver #-}
canonicalOver :: Int -> (Int, Int) -> (Int, Int) -> Bool
canonicalOver _ (!a, !b) (!c, !d) =
    min a b < min c d
--    let sa = min (abs $ a - b) $ n - abs (a - b)
--        sb = min (abs $ c - d) $ n - abs (c - d)
--    in (sa, min a b) < (sb, min c d)


restoreBasicTangle :: UArray Int Int -> NonAlternatingTangle
restoreBasicTangle !chordDiagram =
    let cdl = 1 + snd (bounds chordDiagram)

        restore :: UArray Int Int -> Array Int (Int, Int) -> [Int] -> NonAlternatingTangle
        restore _ _ [] = error "impossible happened"
        restore a h (i : rest) = case () of
            _ | l == 0                           -> emptyTangle
              | l == 2                           -> identityTangle
              | i' == j                          ->
                  let tangle = restore
                          (listArray (0, l - 3) $ map (\ x -> ((a ! ((i + 2 + x) `mod` l)) - i - 2) `mod` l) [0 .. l - 3])
                          (listArray (0, l - 3) $ map (\ x -> h ! ((i + 2 + x) `mod` l)) [0 .. l - 3])
                          [0 .. l - 3]
                  in rotateTangle i $ glueTangles 0 (firstLeg identityTangle) (lastLeg tangle)
              | haveIntersection (i, i') (j, j') ->
                  let tangle = restore (a // [(i, j'), (j, i'), (i', j), (j', i)]) (h // [(i, h ! j), (j, h ! i)]) [0 .. l - 1]
                  in rotateTangle i $ crossingTangle $ glueToBorder (nthLeg tangle j) 2 $
                      if canonicalOver cdl (h ! i) (h ! j)
                          then overCrossing
                          else underCrossing
              | otherwise                        -> restore a h rest
            where
                l = 1 + snd (bounds a)
                i' = a ! i
                j = (i + 1) `mod` l
                j' = a ! j

    in restore
        chordDiagram
        (listArray (0, cdl - 1) $ map (\ i -> (i, chordDiagram ! i)) [0 .. cdl - 1])
        [0 .. cdl - 1]


data ThreadTag = BorderThread {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Int
               | InternalThread {-# UNPACK #-} !Int {-# UNPACK #-} !Int


tagPassOver :: Int -> ThreadTag -> ThreadTag -> Bool
tagPassOver _ (InternalThread a ai) (InternalThread b bi) = (a, ai) < (b, bi)
tagPassOver _ (InternalThread _ _) (BorderThread _ _) = False
tagPassOver _ (BorderThread _ _) (InternalThread _ _) = True
tagPassOver n (BorderThread a ai) (BorderThread b bi)
    | a == b                = ai < bi
    | haveIntersection a b  = canonicalOver n a b
    | otherwise             = a < b


decomposeTangle :: (SkeinRelation r a) => r -> [(Int, [(Int, Int)], [([(Int, Int)], ArbitraryCrossingState)])] -> a -> NonAlternatingTangle -> ChordDiagramsSum a
decomposeTangle relation path !factor !tangle
    | numberOfFreeLoops tangle > 0  =
        decomposeTangle relation path
            (factor * (circleFactor relation ^ numberOfFreeLoops tangle))
            (changeNumberOfFreeLoops tangle 0)
    | otherwise                     =
        let (n, _, threads) = allThreadsWithMarks tangle

            expectedPassOver =
                let tags :: Array (Dart ArbitraryCrossing) ThreadTag
                    tags = array (dartsRange tangle) $ do
                        (tid, thread) <- threads
                        let make = case thread of
                                []                     -> error "internal error"
                                (h, _) : _ | isDart h  -> InternalThread $ numberOfLegs tangle + tid
                                           | otherwise -> let a = legPlace h
                                                              b = legPlace $ snd $ last thread
                                                          in BorderThread (min a b, max a b)
                        (ord, (a, b)) <- [0 ..] `zip` thread
                        [(a, make $ 2 * ord), (b, make $ 2 * ord + 1)]

                in \ d -> on (tagPassOver $ numberOfLegs tangle) (tags !) d (nextCCW d)

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
                in (if length path >= 29 then trace (show $ explode tangle : path) else id) $ singletonStateSum $ ChordDiagram a $ factor *
                    ((if w >= 0 then twistPFactor else twistNFactor) relation ^ abs w) *
                        (circleFactor relation ^ (n - numberOfLegs tangle `div` 2))

            tryCrossing (c : rest) =
                let [d0, d1, d2, d3] = incidentDarts c
                in if passOver d0 == expectedPassOver d0
                    then tryCrossing rest
                    else concatStateSums
                        [ decomposeTangle relation (explode tangle : path) (factor * smoothLplusFactor relation) $ move tangle $ do
                            modifyC False invertCrossing [c]
                            --greedy [reduce2nd]

                        , decomposeTangle relation (explode tangle : path) (factor * (if isOverCrossing (crossingState c) then smoothLzeroFactor else smoothLinftyFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 2
                                      | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 1
                                      | opposite d0 == d3                      -> connectC [(opposite d1, opposite d2)]
                                      | opposite d1 == d2                      -> connectC [(opposite d0, opposite d3)]
                                      | otherwise                              -> substituteC [(opposite d0, d1), (opposite d3, d2)]
                                maskC [c]
                                greedy [reduce2nd]

                        , decomposeTangle relation (explode tangle : path) (factor * (if isOverCrossing (crossingState c) then smoothLinftyFactor else smoothLzeroFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 2
                                      | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 1
                                      | opposite d0 == d1                      -> connectC [(opposite d2, opposite d3)]
                                      | opposite d3 == d2                      -> connectC [(opposite d0, opposite d1)]
                                      | otherwise                              -> substituteC [(opposite d0, d3), (opposite d1, d2)]
                                maskC [c]
                                greedy [reduce2nd]
                        ]

        in tryCrossing $ allCrossings tangle


bruteForceRotate :: (SkeinRelation r a) => r -> Int -> ChordDiagramsSum a -> ChordDiagramsSum a
bruteForceRotate relation rot
    | rot == 0   = id
    | otherwise  = mapStateSum (\ (ChordDiagram a factor) -> decomposeTangle relation [] factor $ rotateTangle rot $ restoreBasicTangle a)


bruteForceMirror :: (SkeinRelation r a) => r -> ChordDiagramsSum a -> ChordDiagramsSum a
bruteForceMirror relation =
    mapStateSum (\ (ChordDiagram a factor) -> decomposeTangle relation [] factor $ mirrorTangle $ restoreBasicTangle a)


instance StateModel ChordDiagramsSum where
    complexityRank (ChordDiagramsSum _ list) = length list

    projection (ChordDiagramsSum _ list) = do
        ChordDiagram cd x <- list
        return (restoreBasicTangle cd, x)

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

    glueHandle relation !p !preSum @ (ChordDiagramsSum !degree _) = {- trace (printf "%i[%i] ) %i" degree (complexityRank preSum) p) $ -}
        let !p' = (p + 1) `mod` degree

            !subst = listArray (0, degree - 1) $
                if p' == 0
                    then [-1] ++ [0 .. degree - 3] ++ [-1]
                    else [0 .. p - 1] ++ [-1, -1] ++ [p .. degree - 3]

            !postSum = flip mapStateSum preSum $ \ (ChordDiagram x k) ->
                let t = restoreBasicTangle x
                in decomposeTangle relation [explode t] k $
                    rotateTangle (if p == 0 || p' == 0 then 0 else p' + 1 - degree) $
                        glueTangles 2 (nthLeg t p) (firstLeg identityTangle)
        in (subst, postSum)

    connect relation (!p, !sumV @ (ChordDiagramsSum !degreeV _)) (!q, !sumU @ (ChordDiagramsSum !degreeU _)) = {- trace (printf "%i[%i] -- %i[%i]" degreeV (complexityRank sumV) degreeU (complexityRank sumU)) $ -}
        let !substV = listArray (0, degreeV - 1) $
                [0 .. p - 1] ++ [-1] ++ [ i + degreeU - 2 | i <- [p + 1 .. degreeV - 1]]

            !substU = listArray (0, degreeU - 1) $
                [ i + degreeU - q + p - 1 | i <- [0 .. q - 1]] ++ [-1] ++ [ i - q - 1 + p | i <- [q + 1 .. degreeU - 1]]

            !result = flip mapStateSum sumV $ \ (ChordDiagram xa ka) ->
                let ta = restoreBasicTangle xa
                in flip mapStateSum sumU $ \ (ChordDiagram xb kb) ->
                    let tb = restoreBasicTangle xb
                    in decomposeTangle relation [explode ta, explode tb] (ka * kb) $
                        rotateTangle (p + 1 - degreeV) $
                            glueTangles 1 (nthLeg ta p) (nthLeg tb q)
        in (substV, substU, result)

    assemble relation border connections internals global = {- trace "assemble" $ -} runST $ do
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
