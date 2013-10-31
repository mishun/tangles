module Math.Combinatorics.ChordDiagrams.Test
    ( test
    ) where

import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex)
import Data.Array.IArray (elems)
import Control.Monad (forM_)
import Text.Printf
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Combinatorics.ChordDiagrams.Generator


test :: Test
test = testGroup "Chord Diagrams"
    [ testCase "Numbers of non-planar chord diagrams" $
        forM_ [ (0, 1), (1, 0), (2, 1), (3, 2), (4, 7), (5, 29), (6, 196), (7, 1788), (8, 21994) ] $ \ (n, t) ->
            assertEqual (printf "for n = %i" n) t (countChordDiagrams $ generateNonPlanarRaw n)

    , testCase "Numbers of bicolourable non-planar chord diagrams" $
        forM_ [ (0, 1), (1, 0), (2, 0), (3, 1), (4, 1), (5, 4), (6, 9), (7, 43), (8, 198), (9, 1435) ] $ \ (n, t) ->
            assertEqual (printf "for n = %i" n) t (countChordDiagrams $ generateBicolourableNonPlanarRaw n)

    , testCase "Numbers of prime non-planar chord diagrams" $
        forM_ [ (0, 1), (1, 0), (2, 1), (3, 1), (4, 4), (5, 18), (6, 116), (7, 1060), (8, 13019) ] $ \ (n, t) ->
            assertEqual (printf "for n = %i" n) t (countChordDiagrams $ generatePrimeNonPlanarRaw n)

    , testCase "Symmetry group information" $
        forM_ [1 .. 9] $ \ !n ->
            let shift l k =
                    let (h, t) = splitAt k l
                    in t ++ h

            in forM_ (listChordDiagrams $ generateNonPlanarRaw n) $ \ (diagArr, (mirror, period)) -> do
                let diag = elems diagArr
                    p = length diag
                    rev = map (\ i -> p - i) $ reverse diag
                    expectedPeriod = 1 + fromJust (elemIndex diag (map (shift diag) [1 .. p]))
                    expectedMirror = isJust (elemIndex diag $ map (shift rev) [0 .. p - 1])

                assertEqual (printf "%s period" (show diag)) expectedPeriod period
                assertEqual (printf "%s mirror" (show diag)) expectedMirror mirror
    ]
