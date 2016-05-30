module Math.Topology.KnotTh.Invariants.LinkingNumbers
    ( writhe
    , selfWrithe
    , totalSelfWrithe
    , totalSelfWrithe'
    , totalCrossWrithe
    , totalCrossWrithe'
    , linkingNumbersTable
    , linkingNumbersInvariant
    ) where

import Control.Monad (forM_, when)
import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Diagram


writhe :: (OrientedKnotted k k') => Vertex k DiagramCrossing -> Int
writhe v | (dartOrientation d0 == dartOrientation d1) == isPassingOver d0  = 1
         | otherwise                                                       = -1
    where d0 = nthOutcomingDart v 0
          d1 = nextCCW d0


selfWrithe :: (OrientedKnotted k k') => Vertex k DiagramCrossing -> Int
selfWrithe v | dartStrandIndex d0 /= dartStrandIndex d1                        = 0
             | (dartOrientation d0 == dartOrientation d1) == isPassingOver d0  = 1
             | otherwise                                                       = -1
    where d0 = nthOutcomingDart v 0
          d1 = nextCCW d0


totalSelfWrithe :: (OrientedKnotted o n) => o DiagramCrossing -> Int
totalSelfWrithe = sum . map selfWrithe . allVertices


totalSelfWrithe' :: (OrientedKnotted o n) => n DiagramCrossing -> Int
totalSelfWrithe' = totalSelfWrithe . arbitraryOrientation


totalCrossWrithe :: (OrientedKnotted o n) => o DiagramCrossing -> Int
totalCrossWrithe = V.sum . V.imap (\ !i -> UV.sum . UV.imap (\ j w -> if i < j then abs w else 0)) . linkingNumbersTable


totalCrossWrithe' :: (OrientedKnotted o n) => n DiagramCrossing -> Int
totalCrossWrithe' = totalCrossWrithe . arbitraryOrientation


linkingNumbersTable :: (OrientedKnotted k k') => k DiagramCrossing -> V.Vector (UV.Vector Int)
linkingNumbersTable knot =
    let n = numberOfStrands knot

        lns = UV.create $ do
                ln <- UMV.replicate (n * n) 0

                let bump !a !b !dw = do
                        let idx = a * n + b
                        x <- UMV.read ln idx
                        UMV.write ln idx $! x + dw

                forM_ (allVertices knot) $ \ !v -> do
                    let d0 = nthOutcomingDart v 0
                        d1 = nthOutcomingDart v 1
                        w | (dartOrientation d0 == dartOrientation d1) == isPassingOver d0  = 1
                          | otherwise                                                       = -1
                        i0 = dartStrandIndex d0
                        i1 = dartStrandIndex d1
                    bump i0 i1 w
                    when (i0 /= i1) $
                        bump i1 i0 w
                return ln

    in V.generate n $ \ !i -> UV.slice (i * n) n lns


linkingNumbersInvariant :: (OrientedKnotted k k') => k' DiagramCrossing -> [Int]
linkingNumbersInvariant knot =
    let ori = arbitraryOrientation knot
        n = numberOfStrands ori
        ln = linkingNumbersTable ori
        fn = numberOfFreeLoops knot
    in replicate (fn * n + fn * (fn - 1) `div` 2) 0
            ++ sort [abs $ (ln V.! i) UV.! j | i <- [0 .. n - 1], j <- [0 .. i - 1]]
