module Math.Combinatorics.ChordDiagrams.Test
    ( test
    ) where

import Control.Monad (forM_)
import Text.Printf
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Combinatorics.ChordDiagrams.Generator


test :: Test
test = testGroup "Chord Diagrams" $
    [ testCase "Numbers of non-planar chord diagrams" $
        forM_ [ (0, 1), (1, 0), (2, 1), (3, 2), (4, 7), (5, 29), (6, 196), (7, 1788), (8, 21994) ] $ \ (n, t) ->
            assertEqual (printf "for n = %i" n) t (generateNonPlanar n (\ c _ -> return $! c + 1) (0 :: Int))
    ]
