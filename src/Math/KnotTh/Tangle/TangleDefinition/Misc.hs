module Math.KnotTh.Tangle.TangleDefinition.Misc
    ( gridTangle
    , chainTangle
    ) where

import Text.Printf
import Math.KnotTh.Tangle.TangleDefinition.Tangle


gridTangle :: (CrossingType ct) => (Int, Int) -> ((Int, Int) -> CrossingState ct) -> Tangle ct
gridTangle (n, m) f
    | n < 0      = error $ printf "gridTangle: first dimension %i is negative" n
    | m < 0      = error $ printf "gridTangle: second dimension %i is negative" m
    | otherwise  =
        let border = ([1 .. n] `zip` repeat 0) ++ (map (\ i -> n * i) [1 .. m] `zip` repeat 1)
                ++ (map (\ i -> n * m + 1 - i) [1 .. n] `zip` repeat 2)
                ++ (map (\ i -> (m - i) * n + 1) [1 .. m] `zip` repeat 3)

            body = do
                j <- [1 .. m]
                i <- [1 .. n]
                return (
                    [ if j > 1 then (n * (j - 2) + i    , 2) else (0, i - 1            )
                    , if i < n then (n * (j - 1) + i + 1, 3) else (0, j + n - 1        )
                    , if j < m then (n * j + i          , 0) else (0, 2 * n + m - i    )
                    , if i > 1 then (n * (j - 1) + i - 1, 1) else (0, 2 * m + 2 * n - j)
                    ], f (i, j))

        in implode (0, border, body)


chainTangle :: (CrossingType ct) => [CrossingState ct] -> Tangle ct
chainTangle [] = zeroTangle
chainTangle list =
    let n = length list
    in implode
        ( 0
        , [(1, 0), (1, 1), (n, 2), (n, 3)]
        , flip map ([1 .. n] `zip` list) $ \ (i, s) ->
            (   [ if i > 1 then (i - 1, 3) else (0, 0)
                , if i > 1 then (i - 1, 2) else (0, 1)
                , if i < n then (i + 1, 1) else (0, 2)
                , if i < n then (i + 1, 0) else (0, 3)
                ]
            , s
            )
        )
