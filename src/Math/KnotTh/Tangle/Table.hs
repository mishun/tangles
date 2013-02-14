module Math.KnotTh.Tangle.Table
    ( emptyTangle
    , identityTangle
    , zeroTangle
    , infinityTangle
    , lonerOverCrossingTangle
    , lonerUnderCrossingTangle
    , altTriangleTangle
    , naTriangleTangle
    , gridTangle
    , groupTangle
    , rationalTangle
    , identityBraidTangle
    , braidGeneratorTangle
    , braidTangle
    ) where

import Text.Printf
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.CascadeCode


lonerOverCrossingTangle :: NonAlternatingTangle
lonerOverCrossingTangle = lonerTangle overCrossing


lonerUnderCrossingTangle :: NonAlternatingTangle
lonerUnderCrossingTangle = lonerTangle underCrossing


altTriangleTangle :: NonAlternatingTangle
altTriangleTangle = decodeCascadeCode [(MU, 0), (XO, 1)]


naTriangleTangle :: NonAlternatingTangle
naTriangleTangle = decodeCascadeCode [(MU, 0), (XU, 1)]


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
                return $! (
                    [ if j > 1 then (n * (j - 2) + i, 2) else (0, i - 1)
                    , if i < n then (n * (j - 1) + i + 1, 3) else (0, j + n - 1)
                    , if j < m then (n * j + i, 0) else (0, 2 * n + m - i)
                    , if i > 1 then (n * (j - 1) + i - 1, 1) else (0, 2 * m + 2 * n - j)
                    ], f (i, j))

        in implode (0, border, body)


groupTangle :: (CrossingType ct) => [CrossingState ct] -> Tangle ct
groupTangle [] = zeroTangle
groupTangle list =
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


rationalTangle :: [Int] -> NonAlternatingTangle
rationalTangle = foldl (\ tangle x ->
        let g = groupTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
        in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
    ) infinityTangle


identityBraidTangle :: (CrossingType ct) => Int -> Tangle ct
identityBraidTangle n
    | n < 0      = error $ printf "identityBraidTangle: requested number of strands %i is negative" n
    | otherwise  =
        let n' = 2 * n - 1
        in implode (0, [(0, n' - i) | i <- [0 .. n']], [])


braidGeneratorTangle :: (CrossingType ct) => Int -> (Int, CrossingState ct) -> Tangle ct
braidGeneratorTangle n (k, s)
    | n < 2               = error $ printf "braidGeneratorTangle: braid must have at least 2 strands, but %s requested" n
    | k < 0 || k > n - 2  = error $ printf "braidGeneratorTangle: generator offset %i is out of bounds (0, %i)" k (n - 2)
    | otherwise           =
        let n' = 2 * n - 1
            k' = n' - k - 1
            b = map $ \ i -> (0, n' - i)
        in implode
            ( 0
            , concat [b [0 .. k - 1], [(1, 0), (1, 1)], b [k + 2 .. k' - 1], [(1, 2), (1, 3)], b [k' + 2 .. n']]
            , [([(0, k), (0, k + 1), (0, k'), (0, k' + 1)], s)]
            )


braidTangle :: (CrossingType ct) => Int -> [(Int, CrossingState ct)] -> Tangle ct
braidTangle n = foldl (\ braid -> (braid |=|) . braidGeneratorTangle n) (identityBraidTangle n)
