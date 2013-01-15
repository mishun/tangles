module Math.Combinatorics.Strings.Lyndon
    ( minimumCyclicShift
    ) where

import Data.Array (listArray, (!))


minimumCyclicShift :: (Ord a) => [a] -> (Int, [a])
minimumCyclicShift list = shift `seq` (shift, drop shift list ++ take shift list)
    where
        n = length list

        a = listArray (0, n - 1) list

        get i = a ! (mod i n)

        grow i j !lyn
            | i + j >= 2 * n  = (j, lyn)
            | otherwise       =
                case compare (get $ i + j) (get $ i + j - lyn) of
                    GT -> grow i (j + 1) (j + 1)
                    EQ -> grow i (j + 1) lyn
                    LT -> (j, lyn)

        scanChunk i
            | ni < n     = scanChunk ni
            | otherwise  = i
            where
                (j, lyn) = grow i 1 1
                ni = i + (j - mod j lyn)

        shift = scanChunk 0
