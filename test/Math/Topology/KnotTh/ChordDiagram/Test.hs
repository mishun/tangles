module Math.Topology.KnotTh.ChordDiagram.Test
    ( test
    ) where

import Control.Monad (forM_)
import Text.Printf
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.ChordDiagram.Lyndon
import Math.Topology.KnotTh.Algebra.Dihedral


naiveMinShift :: (Ord a) => [a] -> [a]
naiveMinShift [] = []
naiveMinShift l = minimum [ let (a, b) = splitAt i l in b ++ a | i <- [0 .. length l]]


minShiftIsOk :: [Int] -> Bool
minShiftIsOk list = snd (minimumCyclicShift list) == naiveMinShift list


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
                    assertEqual (printf "%s period" (show cd)) (naivePeriodOf cd) period
                    let expectedMirror = mirrorIt cd `elem` map (`rotateBy` cd) [0 .. rotationOrder cd - 1]
                    assertEqual (printf "%s mirror" (show cd)) expectedMirror mirror

        , testGroup "String combinatorial functions tests"
            [ testProperty "Minimal cyclic shift" minShiftIsOk

            ,   let list = [6, 4, 3, 4, 5, 3, 3, 1] :: [Int]
                in testCase (printf "Test minimal cyclic shift of %s" $ show list) $
                    snd (minimumCyclicShift list) @?= naiveMinShift list
            ]
        ]
