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
                [ (searchMoves [pass1, pass2],
                    decodeCascadeCode [(WU, 0), (MO, 0)])

                , (searchMoves [pass1, pass2],
                    decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)])

                , (searchMoves [pass1, pass2],
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

                , (searchMoves [pass1, pass2],
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
                [ (searchMoves [flype], decodeCascadeCode [(XO, 0), (XU, 1)])
                ])

        , ("PatternMatchingPerko",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (searchMoves [perko],
                    linkToTangle $ fromGaussCode [[-1, 2, 3, -4, 5, -6, -2, 7, -8, 9, 4, -3, -7, 1, 6, -5, -10, 8, -9, 10]])

                , (searchMoves [perko],
                    mirrorTangle $ linkToTangle $ fromGaussCode [[-1, 2, 3, -4, 5, -6, -2, 7, -8, 9, 4, -3, -7, 1, 6, -5, -10, 8, -9, 10]])

                , (searchMoves [perko],
                    implode
                        ( 0
                        , [(7,0),(4,1),(1,2),(1,3)]
                        ,   [ ([(3,0),(5,1),(0,2),(0,3)],overCrossing)
                            , ([(7,2),(7,1),(4,0),(3,2)],overCrossing)
                            , ([(1,0),(7,3),(2,3),(6,2)],overCrossing)
                            , ([(2,2),(0,1),(5,0),(6,3)],overCrossing)
                            , ([(4,2),(1,1),(6,1),(6,0)],overCrossing)
                            , ([(5,3),(5,2),(3,3),(4,3)],overCrossing)
                            , ([(0,0),(2,1),(2,0),(3,1)],underCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [perko],
                    implode
                        ( 0
                        , [(5,3),(7,1),(2,2),(2,3)]
                        ,   [ ([(3,2),(2,0),(5,2),(6,1)],overCrossing)
                            , ([(1,1),(3,1),(0,2),(0,3)],overCrossing)
                            , ([(7,2),(2,1),(1,0),(6,0)],overCrossing)
                            , ([(5,1),(5,0),(7,0),(6,2)],overCrossing)
                            , ([(4,1),(4,0),(1,2),(0,0)],overCrossing)
                            , ([(3,3),(1,3),(4,3),(7,3)],overCrossing)
                            , ([(4,2),(0,1),(3,0),(6,3)],overCrossing)
                            ]
                        ) :: TangleDiagram)
                ])

        , ("PatternMatchingDoublePass",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (searchMoves [doublePass],
                    implode
                        ( 0
                        , [(6,3),(1,2),(1,3),(7,2)]
                        ,   [ ([(3,3),(4,3),(0,1),(0,2)], overCrossing)
                            , ([(3,1),(3,0),(7,1),(5,2)], overCrossing)
                            , ([(2,1),(2,0),(4,0),(1,0)], overCrossing)
                            , ([(3,2),(5,1),(6,0),(1,1)], overCrossing)
                            , ([(6,1),(4,1),(2,3),(8,0)], overCrossing)
                            , ([(4,2),(5,0),(8,3),(0,0)], overCrossing)
                            , ([(8,1),(2,2),(0,3),(8,2)], underCrossing)
                            , ([(5,3),(7,0),(7,3),(6,2)], underCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [doublePass],
                    implode
                        ( 0
                        ,[(5,2),(6,0),(4,0),(3,0)]
                        ,   [([(4,3),(4,2),(8,1),(6,1)],overCrossing)
                            ,([(5,0),(6,2),(8,0),(8,3)],overCrossing)
                            ,([(0,3),(7,0),(7,3),(4,1)],overCrossing)
                            ,([(0,2),(3,3),(1,1),(1,0)],overCrossing)
                            ,([(2,0),(7,1),(0,0),(6,3)],overCrossing)
                            ,([(0,1),(1,3),(2,1),(5,3)],overCrossing)
                            ,([(3,1),(5,1),(8,2),(3,2)],overCrossing)
                            ,([(2,2),(1,2),(7,2),(2,3)],underCrossing)
                            ]
                        ) :: TangleDiagram)
                ])

        , ("NewMove",
            vcat' with { sep = 0.5 } $ map (uncurry illustratedMove)
                [ (searchMoves [flype, pass1, pass2, pass3, perko, doublePass],
                    implode
                        ( 0
                        , [(8,3),(6,3),(5,3),(3,3)]
                        ,   [ ([(2,0),(7,1),(4,1),(2,1)],overCrossing)
                            , ([(1,0),(1,3),(3,0),(8,2)],underCrossing)
                            , ([(2,2),(4,0),(5,0),(0,3)],underCrossing)
                            , ([(3,1),(1,2),(6,1),(5,1)],underCrossing)
                            , ([(3,2),(4,3),(6,0),(0,2)],overCrossing)
                            , ([(5,2),(4,2),(7,0),(0,1)],underCrossing)
                            , ([(6,2),(1,1),(8,1),(8,0)],underCrossing)
                            , ([(7,3),(7,2),(2,3),(0,0)],underCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [flype, pass1, pass2, pass3, perko, doublePass],
                    implode
                        ( 0
                        , [(8,2),(8,3),(6,3),(4,3)]
                        ,   [ ([(2,0),(5,1),(4,1),(3,1)],overCrossing)
                            , ([(1,0),(3,0),(7,2),(5,2)],underCrossing)
                            , ([(2,1),(1,3),(4,0),(8,1)],underCrossing)
                            , ([(3,2),(1,2),(5,0),(0,3)],overCrossing)
                            , ([(4,2),(1,1),(2,3),(6,0)],underCrossing)
                            , ([(5,3),(7,1),(7,0),(0,2)],overCrossing)
                            , ([(6,2),(6,1),(2,2),(8,0)],underCrossing)
                            , ([(7,3),(3,3),(0,0),(0,1)],underCrossing)
                            ]
                        ) :: TangleDiagram)
                ])
        ]
