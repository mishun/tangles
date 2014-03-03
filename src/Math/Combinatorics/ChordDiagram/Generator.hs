{-# LANGUAGE RankNTypes #-}
module Math.Combinatorics.ChordDiagram.Generator
    ( generateWithCheckerRaw
    , generateAllRaw
    , generateNonPlanarRaw
    , generateBicolourableNonPlanarRaw
    , generateQuasiTreesRaw
    , countChordDiagrams
    , listChordDiagrams
    ) where

import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Applicative ((<$>))
import Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Math.Combinatorics.Strings.Lyndon
import Math.Combinatorics.ChordDiagram.Definition


generateWithCheckerRaw
    :: Int -> (forall s. Int -> UMV.STVector s Int -> Int -> Int -> ST s Bool) ->
        Int -> (forall s. st -> UMV.STVector s Int -> (Bool, Int) -> ST s st)
            -> st -> st

generateWithCheckerRaw minimalChordLength checker n yield initial = runST $ do
    let p = 2 * n

    a <- UMV.replicate p 0
    prev <- UMV.new p
    next <- UMV.new p
    forM_ [0 .. p - 1] $ \ i -> do
        UMV.unsafeWrite prev i $ if i == 0 then p else i - 1
        UMV.unsafeWrite next i $ i + 1
    begin <- newSTRef 0

    let hide !i = do
            nx <- UMV.unsafeRead next i
            pv <- UMV.unsafeRead prev i
            when (nx < p) $ UMV.unsafeWrite prev nx pv
            if pv < p
                then UMV.unsafeWrite next pv nx
                else writeSTRef begin nx

    let restore !i = do
            nx <- UMV.unsafeRead next i
            pv <- UMV.unsafeRead prev i
            when (nx < p) $ UMV.unsafeWrite prev nx i
            if pv < p
                then UMV.unsafeWrite next pv i
                else writeSTRef begin i

    let try !u !v action = do
            ok <- checker p a u v
            when ok $ do
                UMV.unsafeWrite a u (v - u)
                UMV.unsafeWrite a v (p + u - v)
                hide u
                hide v
                _ <- action
                restore v
                restore u
                UMV.unsafeWrite a u 0
                UMV.unsafeWrite a v 0

    result <- newSTRef initial

    let yieldAction !symm = do
            currentState <- readSTRef result
            nextState <- yield currentState a symm
            writeSTRef result $! nextState

    let matchRest !symm !minLength = do
            u <- readSTRef begin
            if u < p
                then do
                    let match !pv = do
                            v <- UMV.unsafeRead next pv
                            when (v < min p (p + u - minLength + 1)) $ do
                                when (v - u >= minLength) $
                                    try u v $ matchRest symm minLength
                                match v
                    match u
                else yieldAction symm

    let backtrack !chordLength !period !mirror = do
            code <- UMV.replicate (3 * p + 1) 0

            let checkMirror !codeLen !newPeriod = case mirror of
                    Nothing         -> backtrack (chordLength + 1) newPeriod Nothing
                    Just mirrorBase -> do
                        dir <- mapM (UMV.unsafeRead code) [0 .. codeLen - 1]
                        rev <- do
                            let base = mod (mirrorBase + p - chordLength) period
                            let start = snd $ fromJust $ find ((> base) . fst) $ zip dir (codeLen - 1 : [0 ..])

                            tmp <- newSTRef []
                            let push !x = modifySTRef' tmp (x :)
                            let iter !i !cross
                                    | i == codeLen  = when cross (push period)
                                    | otherwise     = do
                                        value <- UMV.unsafeRead code $ mod (start + codeLen - i) codeLen
                                        ncross <- if cross && (value <= base || value == period)
                                            then push period >> return False
                                            else return $! cross
                                        if value < period
                                            then do
                                                push $ mod (base + period - value) period
                                                iter (i + 1) ncross
                                            else iter (i + 1) True


                            iter 0 False
                            reverse <$> readSTRef tmp

                        let (shift, minRev) = minimumCyclicShift rev

                        case compare dir minRev of
                            LT -> backtrack (chordLength + 1) newPeriod Nothing
                            EQ -> do
                                let periods = length $ filter (== period) $ take shift rev
                                let nextBase = mod (mod (mirrorBase + p - chordLength) period + periods * (p - period) + chordLength) p
                                backtrack (chordLength + 1) newPeriod (Just $! nextBase)
                            GT -> return ()

            let placeChords !pos !lyn !chunk !lower
                    | chunk == div p period  = when (mod pos lyn == 0) $ checkMirror pos (div (p * lyn) pos)
                    | otherwise              = do
                        lyndonPrev <- if pos == 0
                            then return 0
                            else UMV.unsafeRead code (pos - lyn)

                        let iteration j = do
                                UMV.unsafeWrite code pos j
                                let nextLyn | j == lyndonPrev  = lyn
                                            | otherwise        = pos + 1

                                if j < period
                                    then do
                                        let u = chunk * period + j;
                                        let v = mod (u + chordLength) p

                                        ue <- UMV.unsafeRead a u
                                        ve <- UMV.unsafeRead a v
                                        when (ue == 0 && ve == 0) $
                                            try (min u v) (max u v) $ placeChords (pos + 1) nextLyn chunk (j + 1)

                                        iteration (j + 1)
                                    else placeChords (pos + 1) nextLyn (chunk + 1) 0

                        let bottom = max lyndonPrev lower
                        when (bottom <= period) (iteration bottom)

            full <- (>= p) <$> readSTRef begin
            if full
                then yieldAction (isJust mirror, period)
                else if ((period == p) && isNothing mirror) || (chordLength >= n)
                    then matchRest (isJust mirror, period) chordLength
                    else placeChords 0 1 0 0

    backtrack (max 1 minimalChordLength) 1 (Just 0)
    readSTRef result


generateAllRaw :: Int -> (forall s. st -> UMV.STVector s Int -> (Bool, Int) -> ST s st) -> st -> st
generateAllRaw =
    generateWithCheckerRaw 1 (\ _ _ _ _ -> return True)


generateNonPlanarRaw :: Int -> (forall s. st -> UMV.STVector s Int -> (Bool, Int) -> ST s st) -> st -> st
generateNonPlanarRaw =
    generateWithCheckerRaw 2 (\ _ _ _ _ -> return True)


generateBicolourableNonPlanarRaw :: Int -> (forall s. st -> UMV.STVector s Int -> (Bool, Int) -> ST s st) -> st -> st
generateBicolourableNonPlanarRaw =
    generateWithCheckerRaw 2 (\ _ _ u v -> return $! odd (u + v))


generateQuasiTreesRaw :: Int -> (forall s. st -> UMV.STVector s Int -> (Bool, Int) -> ST s st) -> st -> st
generateQuasiTreesRaw =
    generateWithCheckerRaw 2
        (\ p a u v -> do
            let len = v - u
            pu <- UMV.unsafeRead a $ (u - 1) `mod` p
            nu <- UMV.unsafeRead a (u + 1)
            return $! (pu /= len + 2) && ((nu == 0) || (nu /= len - 2))  
        )


countChordDiagrams :: (Integral i) => ((forall s. i -> UMV.STVector s Int -> (Bool, Int) -> ST s i) -> i -> i) -> i
countChordDiagrams generator =
    generator (\ c _ _ -> return $! c + 1) 0


type CD = (ChordDiagram, (Bool, Int))


listChordDiagrams :: ((forall s. [CD] -> UMV.STVector s Int -> (Bool, Int) -> ST s [CD]) -> [CD] -> [CD]) -> [CD]
listChordDiagrams generator =
    generator  (\ lst diagramST !symmetry -> do
            diagram <- UV.freeze diagramST
            return $! (ChordDiagram diagram, symmetry) : lst
        ) []
