module Math.Combinatorics.ChordDiagram.Definition
    ( ChordDiagram(..)
    , numberOfPoints
    , numberOfChords
    , chordOffsetArray
    , rotateChordDiagram
    , mirrorChordDiagram
    , numberOfCopoints
    , genusOfChordDiagram
    , eulerCharOfChordDiagram
    ) where

import Data.Function (fix)
import Data.Array.IArray (listArray, (!), bounds)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (unless, foldM)


newtype ChordDiagram = ChordDiagram (UArray Int Int) deriving (Eq, Ord, Show)


numberOfPoints :: ChordDiagram -> Int
numberOfPoints (ChordDiagram a) =
    let (_, n) = bounds a
    in n + 1


numberOfChords :: ChordDiagram -> Int
numberOfChords (ChordDiagram a) =
    let (_, n) = bounds a
    in (n + 1) `div` 2


chordOffsetArray :: ChordDiagram -> UArray Int Int
chordOffsetArray (ChordDiagram a) = a


rotateChordDiagram :: Int -> ChordDiagram -> ChordDiagram
rotateChordDiagram rot cd
    | (rot == 0) || (p == 0)  = cd
    | otherwise               =
        ChordDiagram $ listArray (0, p - 1) $
            map (\ !i ->
                    let i' = (i - rot) `mod` p
                    in a ! i'
                ) [0 .. p - 1]
    where
        p = numberOfPoints cd
        a = chordOffsetArray cd


mirrorChordDiagram :: ChordDiagram -> ChordDiagram
mirrorChordDiagram cd =
    let p = numberOfPoints cd
        a = chordOffsetArray cd
    in ChordDiagram $ listArray (0, p - 1) $
        map (\ !i ->
            let i' = (-i) `mod` p
            in p - (a ! i')
        ) [0 .. p - 1]


numberOfCopoints :: ChordDiagram -> Int
numberOfCopoints cd = runST $ do
    let p = numberOfPoints cd
        a = chordOffsetArray cd
    ok <- newArray (0, p - 1) False :: ST s (STUArray s Int Bool)
    foldM (\ !v !i -> do
            oki <- readArray ok i
            if oki
                then return $! v
                else do
                    fix (\ loop !j -> do
                            okj <- readArray ok j
                            unless okj $ do
                                writeArray ok j True
                                loop $ (1 + j + a ! j) `mod` p
                        ) i
                    return $! v + 1
        ) 0 [0 .. p - 1]


genusOfChordDiagram :: ChordDiagram -> Int
genusOfChordDiagram cd = (1 + numberOfChords cd - numberOfCopoints cd) `div` 2


eulerCharOfChordDiagram :: ChordDiagram -> Int
eulerCharOfChordDiagram cd = 1 + numberOfCopoints cd - numberOfChords cd
