{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Moves.Test
    ( testMovesPictures
    ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Draw
import Math.Topology.KnotTh.Moves.Moves


testMovesPictures :: (N b ~ Double, V b ~ V2, Renderable (Path V2 Double) b, Renderable (Text Double) b) => [(String, Diagram b)]
testMovesPictures =
    let illustratedMove move knot =
            let draw = scale 4 . drawKnotDef
                right = hsep 0.5 $ map draw $ move knot
            in draw knot ||| (strutX 8 <> text "->") ||| right

    in  [ ("PatternMatchingPass",
            vsep 0.5 $ map (uncurry illustratedMove)
                [ (searchMoves [pass1, pass2],
                    decodeCascadeCode [(WU, 0), (MO, 0)])

                , (searchMoves [pass1, pass2],
                    decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)])

                , (searchMoves [pass1, pass2],
                    implode
                        ( 0
                        , [(5, 0), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (4, 1), (2, 0), (5, 2)], OverCrossing )
                            , ([(1, 2), (3, 0), (0, 3), (5, 3)], UnderCrossing)
                            , ([(2, 1), (4, 0), (4, 3), (4, 2)], OverCrossing )
                            , ([(3, 1), (1, 1), (3, 3), (3, 2)], OverCrossing )
                            , ([(0, 0), (6, 3), (1, 3), (2, 3)], UnderCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], UnderCrossing)
                            ]
                        ))

                , (searchMoves [pass1, pass2],
                    implode
                        ( 0
                        , [(3, 2), (6, 0), (6, 1), (2, 2)]
                        ,   [ ([(6, 2), (2, 1), (2, 0), (5, 2)], OverCrossing )
                            , ([(1, 2), (1, 1), (0, 3), (5, 3)], UnderCrossing)
                            , ([(4, 1), (4, 0), (0, 0), (4, 2)], OverCrossing )
                            , ([(3, 1), (3, 0), (3, 3), (5, 0)], OverCrossing )
                            , ([(4, 3), (6, 3), (1, 3), (2, 3)], UnderCrossing)
                            , ([(0, 1), (0, 2), (1, 0), (5, 1)], UnderCrossing)
                            ]
                        ))
                ])

        , ("PatternMatchingFlype",
            vsep 0.5 $ map (uncurry illustratedMove)
                [ (searchMoves [flype], decodeCascadeCode [(XO, 0), (XU, 1)])
                ])

        , ("PatternMatchingPerko",
            vsep 0.5 $ map (uncurry illustratedMove)
                [ (searchMoves [perko],
                    toTangle $ fromGaussCode [[-1, 2, 3, -4, 5, -6, -2, 7, -8, 9, 4, -3, -7, 1, 6, -5, -10, 8, -9, 10]])

                , (searchMoves [perko],
                    mirrorIt $ toTangle $ fromGaussCode [[-1, 2, 3, -4, 5, -6, -2, 7, -8, 9, 4, -3, -7, 1, 6, -5, -10, 8, -9, 10]])

                , (searchMoves [perko],
                    implode
                        ( 0
                        , [(7,0),(4,1),(1,2),(1,3)]
                        ,   [ ([(3,0),(5,1),(0,2),(0,3)],OverCrossing)
                            , ([(7,2),(7,1),(4,0),(3,2)],OverCrossing)
                            , ([(1,0),(7,3),(2,3),(6,2)],OverCrossing)
                            , ([(2,2),(0,1),(5,0),(6,3)],OverCrossing)
                            , ([(4,2),(1,1),(6,1),(6,0)],OverCrossing)
                            , ([(5,3),(5,2),(3,3),(4,3)],OverCrossing)
                            , ([(0,0),(2,1),(2,0),(3,1)],UnderCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [perko],
                    implode
                        ( 0
                        , [(5,3),(7,1),(2,2),(2,3)]
                        ,   [ ([(3,2),(2,0),(5,2),(6,1)],OverCrossing)
                            , ([(1,1),(3,1),(0,2),(0,3)],OverCrossing)
                            , ([(7,2),(2,1),(1,0),(6,0)],OverCrossing)
                            , ([(5,1),(5,0),(7,0),(6,2)],OverCrossing)
                            , ([(4,1),(4,0),(1,2),(0,0)],OverCrossing)
                            , ([(3,3),(1,3),(4,3),(7,3)],OverCrossing)
                            , ([(4,2),(0,1),(3,0),(6,3)],OverCrossing)
                            ]
                        ) :: TangleDiagram)
                ])

        , ("PatternMatchingDoublePass",
            vsep 0.5 $ map (uncurry illustratedMove)
                [ (searchMoves [doublePass],
                    implode
                        ( 0
                        , [(6,3),(1,2),(1,3),(7,2)]
                        ,   [ ([(3,3),(4,3),(0,1),(0,2)], OverCrossing)
                            , ([(3,1),(3,0),(7,1),(5,2)], OverCrossing)
                            , ([(2,1),(2,0),(4,0),(1,0)], OverCrossing)
                            , ([(3,2),(5,1),(6,0),(1,1)], OverCrossing)
                            , ([(6,1),(4,1),(2,3),(8,0)], OverCrossing)
                            , ([(4,2),(5,0),(8,3),(0,0)], OverCrossing)
                            , ([(8,1),(2,2),(0,3),(8,2)], UnderCrossing)
                            , ([(5,3),(7,0),(7,3),(6,2)], UnderCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [doublePass],
                    implode
                        ( 0
                        ,[(5,2),(6,0),(4,0),(3,0)]
                        ,   [([(4,3),(4,2),(8,1),(6,1)],OverCrossing)
                            ,([(5,0),(6,2),(8,0),(8,3)],OverCrossing)
                            ,([(0,3),(7,0),(7,3),(4,1)],OverCrossing)
                            ,([(0,2),(3,3),(1,1),(1,0)],OverCrossing)
                            ,([(2,0),(7,1),(0,0),(6,3)],OverCrossing)
                            ,([(0,1),(1,3),(2,1),(5,3)],OverCrossing)
                            ,([(3,1),(5,1),(8,2),(3,2)],OverCrossing)
                            ,([(2,2),(1,2),(7,2),(2,3)],UnderCrossing)
                            ]
                        ) :: TangleDiagram)
                ])

        , ("NewMove",
            vsep 0.5 $ map (uncurry illustratedMove)
                [ (searchMoves [flype, pass1, pass2, pass3, perko, doublePass],
                    implode
                        ( 0
                        , [(8,3),(6,3),(5,3),(3,3)]
                        ,   [ ([(2,0),(7,1),(4,1),(2,1)],OverCrossing)
                            , ([(1,0),(1,3),(3,0),(8,2)],UnderCrossing)
                            , ([(2,2),(4,0),(5,0),(0,3)],UnderCrossing)
                            , ([(3,1),(1,2),(6,1),(5,1)],UnderCrossing)
                            , ([(3,2),(4,3),(6,0),(0,2)],OverCrossing)
                            , ([(5,2),(4,2),(7,0),(0,1)],UnderCrossing)
                            , ([(6,2),(1,1),(8,1),(8,0)],UnderCrossing)
                            , ([(7,3),(7,2),(2,3),(0,0)],UnderCrossing)
                            ]
                        ) :: TangleDiagram)

                , (searchMoves [flype, pass1, pass2, pass3, perko, doublePass],
                    implode
                        ( 0
                        , [(8,2),(8,3),(6,3),(4,3)]
                        ,   [ ([(2,0),(5,1),(4,1),(3,1)],OverCrossing)
                            , ([(1,0),(3,0),(7,2),(5,2)],UnderCrossing)
                            , ([(2,1),(1,3),(4,0),(8,1)],UnderCrossing)
                            , ([(3,2),(1,2),(5,0),(0,3)],OverCrossing)
                            , ([(4,2),(1,1),(2,3),(6,0)],UnderCrossing)
                            , ([(5,3),(7,1),(7,0),(0,2)],OverCrossing)
                            , ([(6,2),(6,1),(2,2),(8,0)],UnderCrossing)
                            , ([(7,3),(3,3),(0,0),(0,1)],UnderCrossing)
                            ]
                        ) :: TangleDiagram)
                ])
        ]
