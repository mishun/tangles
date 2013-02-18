module Math.KnotTh.Tangle.TangleDefinition.Braid
    ( (|=|)
    , identityBraidTangle
    , braidGeneratorTangle
    , braidTangle
    , reversingBraidTangle
    ) where

import Text.Printf
import Math.KnotTh.Tangle.TangleDefinition.Class
import Math.KnotTh.Tangle.TangleDefinition.Tangle


(|=|) :: (CrossingType ct) => Tangle ct -> Tangle ct -> Tangle ct
(|=|) a b
    | al /= bl   = error $ printf "braidLikeGlue: different numbers of legs (%i and %i)" al bl
    | otherwise  = glueTangles n (nthLeg a n) (nthLeg b (n - 1))
    where
        al = numberOfLegs a
        bl = numberOfLegs b
        n = al `div` 2


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


reversingBraidTangle :: (CrossingType ct) => Int -> CrossingState ct -> Tangle ct
reversingBraidTangle n s
    | n < 0      = error $ printf "flipBraidTangle: requested number of strands %i is negative" n
    | otherwise  = braidTangle n $ do
        k <- [2 .. n]
        i <- [0 .. n - k]
        return (i, s)
