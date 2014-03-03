module Math.Combinatorics.ChordDiagram.Definition
    ( ChordDiagram(..)
    , numberOfPoints
    , numberOfChords
    , chordOffsetArray
    , chordSpan
    , chordEnd
    , isDiameterChord
    , rotateChordDiagram
    , mirrorChordDiagram
    , numberOfCopoints
    , genusOfChordDiagram
    , eulerCharOfChordDiagram
    ) where

import Data.Function (fix)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Control.Monad.ST (runST)
import Control.Monad (unless, foldM)


newtype ChordDiagram = ChordDiagram (UV.Vector Int) deriving (Eq, Ord, Show)


numberOfPoints :: ChordDiagram -> Int
numberOfPoints (ChordDiagram a) = UV.length a


numberOfChords :: ChordDiagram -> Int
numberOfChords (ChordDiagram a) = UV.length a `div` 2


chordOffsetArray :: ChordDiagram -> UV.Vector Int
chordOffsetArray (ChordDiagram a) = a


chordSpan :: ChordDiagram -> Int -> Int
chordSpan cd@(ChordDiagram a) x =
    let p = numberOfPoints cd
    in a UV.! (x `mod` p)


chordEnd :: ChordDiagram -> Int -> Int
chordEnd cd x =
    (x + chordSpan cd x) `mod` numberOfPoints cd


isDiameterChord :: ChordDiagram -> Int -> Bool
isDiameterChord cd x =
    chordSpan cd x == numberOfChords cd


rotateChordDiagram :: Int -> ChordDiagram -> ChordDiagram
rotateChordDiagram rot cd
    | (rot == 0) || (p == 0)  = cd
    | otherwise               =
        ChordDiagram $ UV.generate p $ \ !i ->
            let i' = (i - rot) `mod` p
            in a UV.! i'
    where
        p = numberOfPoints cd
        a = chordOffsetArray cd


mirrorChordDiagram :: ChordDiagram -> ChordDiagram
mirrorChordDiagram cd =
    let p = numberOfPoints cd
        a = chordOffsetArray cd
    in ChordDiagram $ UV.generate p $ \ !i ->
        let i' = (-i) `mod` p
        in p - (a UV.! i')


numberOfCopoints :: ChordDiagram -> Int
numberOfCopoints cd = runST $ do
    let p = numberOfPoints cd
        a = chordOffsetArray cd
    ok <- UMV.replicate p False
    foldM (\ !v !i -> do
            oki <- UMV.read ok i
            if oki
                then return $! v
                else do
                    fix (\ loop !j -> do
                            okj <- UMV.read ok j
                            unless okj $ do
                                UMV.write ok j True
                                loop $ (1 + j + a UV.! j) `mod` p
                        ) i
                    return $! v + 1
        ) 0 [0 .. p - 1]


genusOfChordDiagram :: ChordDiagram -> Int
genusOfChordDiagram cd = (1 + numberOfChords cd - numberOfCopoints cd) `div` 2


eulerCharOfChordDiagram :: ChordDiagram -> Int
eulerCharOfChordDiagram cd = 1 + numberOfCopoints cd - numberOfChords cd
