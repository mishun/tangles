{-# LANGUAGE TemplateHaskell #-}
module Math.Topology.KnotTh.Knotted.TH.Show
    ( produceShowDart
    , produceShowVertex
    , produceShowKnotted
    ) where

import Language.Haskell.TH
import Text.Printf
import Math.Topology.KnotTh.Knotted


produceShowDart :: Name -> (ExpQ -> [(ExpQ, ExpQ)]) -> DecsQ
produceShowDart knotType extraGuards = (:[]) `fmap` do
    a <- newName "a"
    instanceD (cxt []) [t| Show (Dart $(conT knotType) $(varT a)) |] -- (conT ''Show `appT` (conT dartN `appT` conT knotN `appT` ct))
        [ funD 'show $ (:[]) $ do
            d <- newName "d"
            clause [varP d] (guardedB $ map (uncurry normalGE) $ extraGuards (varE d) ++
                    [ ([| otherwise |],
                        [|  let (c, p) = beginPair' $(varE d)
                            in printf "(Dart %i %i)" c p
                        |])
                    ]
                ) []
        ]


produceShowVertex :: Name -> DecsQ
produceShowVertex knotType =
    [d| instance (Show a) => Show (Vertex $(conT knotType) a) where
            show v =
                printf "(Crossing %i %s [ %s ])"
                    (vertexIndex v)
                    (show $ vertexCrossing v)
                    (unwords $ map (show . opposite) $ outcomingDarts v)
    |]


produceShowKnotted :: Name -> DecsQ
produceShowKnotted knotType =
    [d| instance (Show a) => Show ($(conT knotType) a) where
            show k =
                printf "(%s (%i O) %s)"
                    $(litE $ stringL $ nameBase knotType)
                    (numberOfFreeLoops k)
                    (unwords $ map show $ allVertices k)
    |] 
