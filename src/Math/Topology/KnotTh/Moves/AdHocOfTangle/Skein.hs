module Math.Topology.KnotTh.Moves.AdHocOfTangle.Skein
    ( smoothA
    , smoothB
    ) where

import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move


smoothA :: TangleDiagramCrossing -> MoveM s DiagramCrossing ()
smoothA cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    case () of
        _ | od0 == d1 && od3 == d2 -> emitCircle 2
          | od0 == d3 && od1 == d2 -> emitCircle 1
          | od0 == d3              -> connectC [(od1, od2)]
          | od1 == d2              -> connectC [(od0, od3)]
          | otherwise              -> substituteC [(od0, d1), (od3, d2)]
    maskC [cs]


smoothB :: TangleDiagramCrossing -> MoveM s DiagramCrossing ()
smoothB cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    case () of
        _ | od0 == d3 && od1 == d2 -> emitCircle 2
          | od0 == d1 && od3 == d2 -> emitCircle 1
          | od0 == d1              -> connectC [(od2, od3)]
          | od3 == d2              -> connectC [(od0, od1)]
          | otherwise              -> substituteC [(od0, d3), (od1, d2)]
    maskC [cs]
