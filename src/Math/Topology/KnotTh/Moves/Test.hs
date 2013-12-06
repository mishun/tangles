module Math.Topology.KnotTh.Moves.Test
    ( testMovesPictures
    ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Draw
import qualified Math.Topology.KnotTh.Moves.AdHocOfTangle.Pass as PassT
import Math.Topology.KnotTh.Moves.Moves


testMovesPictures :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) => [(String, Diagram b R2)]
testMovesPictures =
    let illustratedMove move knot =
            let draw = scale 4 . freeze . drawKnotDef
                right = hcat' with { sep = 0.5 } $ map draw $ move knot
                arrow = strutX 8 <> text "->"
            in draw knot ||| arrow ||| right

    in  [ ("AdHocTanglePass",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (PassT.neighbours, decodeCascadeCode [(WU, 0), (MO, 0)])
                , (PassT.neighbours, decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)])

                , (PassT.neighbours,
                    implode
                        ( 0
                        , [(5, 0), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (4, 1), (2, 0), (5, 2)], overCrossing )
                            , ([(1, 2), (3, 0), (0, 3), (5, 3)], underCrossing)
                            , ([(2, 1), (4, 0), (4, 3), (4, 2)], overCrossing )
                            , ([(3, 1), (1, 1), (3, 3), (3, 2)], overCrossing )
                            , ([(0, 0), (6, 3), (1, 3), (2, 3)], underCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], underCrossing)
                            ]
                        ))

                , (PassT.neighbours,
                    implode
                        ( 0
                        , [(3, 2), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (2, 1), (2, 0), (5, 2)], overCrossing )
                            , ([(1, 2), (1, 1), (0, 3), (5, 3)], underCrossing)
                            , ([(4, 1), (4, 0), (0, 0), (4, 2)], overCrossing )
                            , ([(3, 1), (3, 0), (3, 3), (5, 0)], overCrossing )
                            , ([(4, 3), (6, 3), (1, 3), (2, 3)], underCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], underCrossing)
                            ]
                        ))
                ])

        , ("PatternMatchingPass",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (patternMatching [pass2], decodeCascadeCode [(WU, 0), (MO, 0)])
                , (patternMatching [pass2], decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)])

                , (patternMatching [pass2],
                    implode
                        ( 0
                        , [(5, 0), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (4, 1), (2, 0), (5, 2)], overCrossing )
                            , ([(1, 2), (3, 0), (0, 3), (5, 3)], underCrossing)
                            , ([(2, 1), (4, 0), (4, 3), (4, 2)], overCrossing )
                            , ([(3, 1), (1, 1), (3, 3), (3, 2)], overCrossing )
                            , ([(0, 0), (6, 3), (1, 3), (2, 3)], underCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], underCrossing)
                            ]
                        ))

                , (patternMatching [pass2],
                    implode
                        ( 0
                        , [(3, 2), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (2, 1), (2, 0), (5, 2)], overCrossing )
                            , ([(1, 2), (1, 1), (0, 3), (5, 3)], underCrossing)
                            , ([(4, 1), (4, 0), (0, 0), (4, 2)], overCrossing )
                            , ([(3, 1), (3, 0), (3, 3), (5, 0)], overCrossing )
                            , ([(4, 3), (6, 3), (1, 3), (2, 3)], underCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], underCrossing)
                            ]
                        ))
                ])

        , ("PatternMatchingFlype",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (patternMatching [flype], decodeCascadeCode [(XO, 0), (XU, 1)])
                ])

        , ("PatternMatchingPerko",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (map tangleToLink . patternMatching [perko] . linkToTangle,
                    fromGaussCode [[-1, 2, 3, -4, 5, -6, -2, 7, -8, 9, 4, -3, -7, 1, 6, -5, -10, 8, -9, 10]])
                ])
        ]
