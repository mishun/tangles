module Math.Topology.KnotTh.Tabulation.LinkDiagrams
    ( nextGeneration
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V
import Math.Topology.KnotTh.Link


p0 :: a -> Dart Link a -> Dart Link a
p0 cross ab =
    let link = dartOwner ab
        ba = opposite ab
        cd = nextCCW ab
        dc = opposite cd
        n = 1 + numberOfVertices link

        res = implode
            ( numberOfFreeLoops link
            , let opp' x | x == ab    = (n, 0)
                         | x == ba    = (n, 1)
                         | x == dc    = (n, 2)
                         | x == cd    = (n, 3)
                         | otherwise  = endPair' x
              in map (\ v -> (map opp' $ outcomingDarts v, vertexCrossing v)) (allVertices link)
                     ++ [(map beginPair' [ab, ba, dc, cd], cross)]
            )

    in nthOutcomingDart
        (nthVertex res $ beginVertexIndex ab)
        (beginPlace ab)


p1 :: a -> Dart Link a -> Dart Link a
p1 cross ab =
    let link = dartOwner ab
        ba = opposite ab
        ac = nextCCW ab
        ca = opposite ac
        bd = nextCW ba
        db = opposite bd
        n = 1 + numberOfVertices link

        res = implode
            ( numberOfFreeLoops link
            , let opp' x | x == ac    = (n, 0)
                         | x == bd    = (n, 1)
                         | x == db    = (n, 2)
                         | x == ca    = (n, 3)
                         | otherwise  = endPair' x
              in map (\ v -> (map opp' $ outcomingDarts v, vertexCrossing v)) (allVertices link)
                     ++ [(map beginPair' [ac, bd, db, ca], cross)]
            )

    in nthOutcomingDart
        (nthVertex res $ beginVertexIndex ab)
        (beginPlace ab)


tryDescent :: [a] -> Link a -> [Dart Link a]
tryDescent cross link = do
    c <- cross
    d <- allHalfEdges link
    p0 c d : [p1 c d | opposite (nextCCW d) /= nextCW (opposite d)]


tryAscent :: Dart Link a -> Maybe (Link a, V.Vector Int)
tryAscent a = do
    let b = nextCCW a
    return (dartOwner a, undefined)


nextGeneration :: [a] -> Link a -> [Link a]
nextGeneration cross link =
    map fst $ mapMaybe tryAscent $ tryDescent cross link
