module Math.Combinatorics.ChordDiagrams.Test
    ( test
    ) where

import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex)
import Data.Array.IArray (elems)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray)
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
            assertEqual (printf "for n = %i" n) t (generateNonPlanar n (\ c _ _ -> return $! c + 1) (0 :: Int))

    , testCase "Symmetry group information" $
        forM_ [1 .. 6] $ \ !n ->
            let list :: [(UArray Int Int, (Bool, Int))]
                list = generateNonPlanar 5
                    (\ lst diagST symm -> do
                            diag <- freeze diagST
                            return $! (diag, symm) : lst
                    ) []

                shift l k =
                    let (h, t) = splitAt k l
                    in t ++ h

                getPeriod l = 1 + fromJust (elemIndex l (map (shift l) [1 .. length l])) 

            in forM_ list $ \ (diagArr, (mirror, period)) -> do
                let diag = elems diagArr
                    rev = map (\ i -> 2 * n - i) $ reverse diag

                assertEqual (printf "%s period" (show diag)) (getPeriod diag) period
                assertEqual (printf "%s mirror" (show diag)) mirror
                    (isJust (elemIndex diag $ map (shift rev) [0 .. 2 * n - 1]))
    ]
