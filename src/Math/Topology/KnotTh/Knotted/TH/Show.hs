{-# LANGUAGE TemplateHaskell #-}
module Math.Topology.KnotTh.Knotted.TH.Show
    ( produceShowDart
    , produceShowCrossing
    , produceShowKnot
    ) where

import Language.Haskell.TH
import Text.Printf
import Math.Topology.KnotTh.Knotted


produceShowDart :: Name -> Name -> (ExpQ -> [(ExpQ, ExpQ)]) -> DecsQ
produceShowDart knotN dartN extraGuards = (:[]) `fmap` do
    ct <- varT `fmap` newName "ct"
    instanceD (cxt []) (conT ''Show `appT` (conT dartN `appT` conT knotN `appT` ct))
        [ funD 'show $ (:[]) $ do
            d <- newName "d"
            clause [varP d] (guardedB $ map (uncurry normalGE) $ extraGuards (varE d) ++
                    [ ([| otherwise |],
                        [|  let (c, p) = begin $(varE d)
                            in printf "(Dart %i %i)" (crossingIndex c) p
                        |])
                    ]
                ) []
        ]


produceShowCrossing :: Name -> Name -> DecsQ
produceShowCrossing knotN crosN = (:[]) `fmap` do
    ct <- varT `fmap` newName "ct"
    instanceD (cxt [classP ''Show [ct], classP ''CrossingType [ct]]) (conT ''Show `appT` (conT crosN `appT` conT knotN `appT` ct))
        [ valD (varP 'show) (normalB
                [| \ c ->
                    printf "(Crossing %i %s [ %s ])"
                        (crossingIndex c)
                        (show $ crossingState c)
                        (unwords $ map (show . opposite) $ incidentDarts c)
                |]
            ) []
        ]


produceShowKnot :: Name -> DecsQ
produceShowKnot knotN = (:[]) `fmap` do
    ct <- varT `fmap` newName "ct"
    instanceD (cxt [classP ''Show [ct], classP ''CrossingType [ct]]) (conT ''Show `appT` (conT knotN `appT` ct))
        [ valD (varP 'show) (normalB
                [| \ knot ->
                    printf "(%s (%i O) %s)"
                        $(litE $ stringL $ nameBase knotN)
                        (numberOfFreeLoops knot)
                        (unwords $ map show $ allCrossings knot)
                |]
            ) []
        ]
