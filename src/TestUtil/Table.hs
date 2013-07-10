{-# LANGUAGE Rank2Types #-}
module TestUtil.Table
    ( Table
    , generateTable
    , generateTable'
    , testTable
    , testTable'
    , printTable
    ) where

import Test.HUnit
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, foldl')
import qualified Data.Map as M
import Control.Monad.State.Strict (execState, get, put)
import Control.Monad (forM_, guard)
import Text.Printf
import System.CPUTime (getCPUTime)
import Math.Algebra.Group.Dn (DnSubGroup, hasReflectionPart, rotationPeriod)
import Math.KnotTh.Tangle


type Table = M.Map (Int, Int) Int


generateTable :: Bool -> (forall m. (Monad m) => (Tangle ct -> DnSubGroup -> m ()) -> m ()) -> Table
generateTable isLabelled generator =
    let yield !tangle !symmetry = do
            let w | isLabelled  = rotationPeriod symmetry * (if hasReflectionPart symmetry then 1 else 2)
                  | otherwise   = 1
            get >>= (\ !m -> put $! M.insertWith' (+) (numberOfCrossings tangle, numberOfLegs tangle) w m)
    in execState (generator yield) M.empty


generateTable' :: (forall m. (Monad m) => (Tangle ct -> m ()) -> m ()) -> Table
generateTable' generator =
    let yield !tangle =
            get >>= (\ !m -> put $! M.insertWith (+) (numberOfCrossings tangle, numberOfLegs tangle) 1 m)
    in execState (generator yield) M.empty


testTable :: (Int -> Table) -> [[Int]] -> Assertion
testTable getTable = testTable' (return . getTable)


testTable' :: (Int -> IO Table) -> [[Int]] -> Assertion
testTable' getTable targetList = do
    let target = M.fromList $ do
            (c, xs) <- zip [1 ..] targetList
            (l, n) <- zip [4, 6 ..] xs
            guard $ n /= 0
            return ((c, l), n)

    table <- getTable $ length targetList
    forM_ (M.assocs table) $ \ ((c, l), actual) ->
        assertEqual (printf "for c = %i and l = %i" c l) (M.lookup (c, l) target) (Just actual)
    assertEqual "Tables are different" target table


printTable :: String -> Table -> IO ()
printTable name table = do
    beginTime <- getCPUTime
    putStrLn $ name ++ ":"
    let maxN = maximum $ map fst $ M.keys table
    let totalTangles = sum $ M.elems table
    putStr $
        let possibleLegs = [ 2 * (i + 1) | i <- [0 .. maxN] ]
            total = "total: " ++ show totalTangles
            header = intercalate "\t" $ "l\\n" : map show [1 .. maxN]
            line l = intercalate "\t" $ show l : map (\ c -> cell (c, l)) [1 .. maxN]

            allCr =
                let numForCr c = foldl' (\ !carry ((!cc, !_), !cn) -> carry + if cc == c then cn else 0) 0 $ M.assocs table
                in intercalate "\t" $ "all:" : map (show . numForCr) [1 .. maxN]

            cell arg
                | isJust r && x > 0  = show x
                | otherwise          = "."
                where
                    r = M.lookup arg table
                    x = fromJust r
        in unlines $ [header] ++ map line possibleLegs ++ [allCr, total]
    endTime <- getCPUTime
    let time = (fromInteger (endTime - beginTime) :: Double) / 1.0e12
    printf "Time = %fs (%f tangles/s)\n" time (realToFrac totalTangles / time)
