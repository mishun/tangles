module Math.Topology.KnotTh.ChordDiagram.Test
    ( test
    ) where

import Control.Monad (forM_)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)
import Text.Printf
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.ChordDiagram


test :: Test
test =
    let testGenerator gen list =
            forM_ list $ \ (n, expected) ->
                let total = countChordDiagrams (gen n) :: Int
                in assertEqual (printf "for n = %i" n) expected total

    in testGroup "Chord Diagrams"
        [ testCase "Numbers of non-planar chord diagrams" $
            testGenerator generateNonPlanarRaw
                [ (0, 1), (1, 0), (2, 1), (3, 2), (4, 7), (5, 29), (6, 196), (7, 1788), (8, 21994) ]

        , testCase "Numbers of bicolourable non-planar chord diagrams" $
            testGenerator generateBicolourableNonPlanarRaw
                [ (0, 1), (1, 0), (2, 0), (3, 1), (4, 1), (5, 4), (6, 9), (7, 43), (8, 198), (9, 1435) ]

        , testCase "Numbers of quasi-tree chord diagrams" $
            testGenerator generateQuasiTreesRaw
                [ (0, 1), (1, 0), (2, 1), (3, 1), (4, 4), (5, 18), (6, 116), (7, 1060), (8, 13019) ]

        , testCase "Symmetry group information" $
            forM_ [1 .. 9] $ \ !n ->
                forM_ (listChordDiagrams $ generateNonPlanarRaw n) $ \ (cd, (mirror, period)) -> do
                    let p = numberOfChordEnds cd
                        expectedPeriod = 1 + fromJust (elemIndex cd $ map (`rotateChordDiagram` cd) [1 .. p])
                        expectedMirror = isJust (elemIndex (mirrorChordDiagram cd) $ map (`rotateChordDiagram` cd) [0 .. p - 1])

                    assertEqual (printf "%s period" (show cd)) expectedPeriod period
                    assertEqual (printf "%s mirror" (show cd)) expectedMirror mirror
        ]
