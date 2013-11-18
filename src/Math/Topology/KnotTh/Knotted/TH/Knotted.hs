{-# LANGUAGE TemplateHaskell #-}
module Math.Topology.KnotTh.Knotted.TH.Knotted
    ( KnottedSettings(..)
    , ImplodeExplodeSettings(..)
    , defaultKnotted
    , defaultImplodeExplode
    , produceKnotted
    ) where

import Language.Haskell.TH
import Data.Ix (Ix(..))
import Data.List (foldl')
import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.Array.IArray (listArray, bounds, amap)
import Data.Array.MArray (newArray_)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STArray, STUArray)
import Control.Arrow (first)
import Control.Monad.ST (ST, runST)
import Control.Monad.Writer (Writer, WriterT, execWriter, execWriterT, tell)
import Control.Monad ((>=>), when, forM_)
import Control.Monad.Trans (lift)
import Control.DeepSeq
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted


data KnottedSettings = KnottedSettings
    { modifyNumberOfEdges          :: Maybe (ExpQ -> ExpQ -> ExpQ)
    , modifyIsDart                 :: Maybe ((ExpQ, ExpQ) -> ExpQ)
    , modifyNextCCW                :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
    , modifyNextCW                 :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
    , modifyDartPlace              :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
    , modifyIncidentCrossing       :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
    , modifyFoldMIncidentDartsFrom :: Maybe ((ExpQ, (ExpQ, ExpQ)) -> ExpQ -> ExpQ)
    , implodeExplodeSettings       :: ImplodeExplodeSettings
    , emptyExtraInitializers       :: [Q (Name, Exp)]
    }


data ImplodeExplodeSettings = ImplodeExplodeSettings
    { extraImplodeExplodeParams :: [(Name, TypeQ, ExpQ -> ExpQ)]
    , extraImplodePairCases     :: [(String -> [ExpQ] -> ExpQ) -> ExpQ -> ExpQ -> ExpQ -> (ExpQ, ExpQ)]
    , extraExplodePairCases     :: [ExpQ -> (ExpQ, ExpQ)]
    , modifyImplodeLimit        :: Maybe (ExpQ -> ExpQ -> ExpQ)
    , implodePreExtra           :: (String -> [ExpQ] -> ExpQ) -> [StmtQ]
    , implodePostExtra          :: ExpQ -> ExpQ -> (ExpQ -> (ExpQ, ExpQ) -> ExpQ) -> [StmtQ]
    , implodeInitializers       :: [Q (Name, Exp)]
    }


defaultKnotted :: KnottedSettings
defaultKnotted = KnottedSettings
    { modifyNumberOfEdges          = Nothing
    , modifyIsDart                 = Nothing
    , modifyNextCCW                = Nothing
    , modifyNextCW                 = Nothing
    , modifyDartPlace              = Nothing
    , modifyIncidentCrossing       = Nothing
    , modifyFoldMIncidentDartsFrom = Nothing
    , implodeExplodeSettings       = defaultImplodeExplode
    , emptyExtraInitializers       = []
    }


defaultImplodeExplode :: ImplodeExplodeSettings
defaultImplodeExplode = ImplodeExplodeSettings
    { extraImplodeExplodeParams = []
    , extraImplodePairCases     = []
    , extraExplodePairCases     = []
    , modifyImplodeLimit        = Nothing
    , implodePreExtra           = const []
    , implodePostExtra          = const $ const $ const []
    , implodeInitializers       = []
    }


declare :: Q Dec -> WriterT [Dec] Q ()
declare = lift >=> tell . (:[])


append :: a -> Writer [a] ()
append = tell . (: [])


produceKnotted :: DecsQ -> KnottedSettings -> DecsQ
produceKnotted knotPattern inst = execWriterT $ do
    [DataD [] knotTN [PlainTV crossType] [RecC knotCN knotFields] []] <- lift knotPattern

    let dartN = mkName "Dart"
        vertN = mkName "Vertex"
        nameL = litE $ stringL $ nameBase knotTN

    let loopsCount = mkName "loopsCount"
        vertexCount = mkName "vertexCount"
        stateArray = mkName "stateArray"
        connsArray = mkName "connsArray"

    let ies = implodeExplodeSettings inst

    declare $ dataD (cxt []) knotTN [PlainTV crossType] [recC knotCN $
        [ (,,) loopsCount  Unpacked `fmap` [t| Int |]
        , (,,) vertexCount Unpacked `fmap` [t| Int |]
        , (,,) connsArray  Unpacked `fmap` [t| UArray Int Int |]
        , (,,) stateArray  Unpacked `fmap` [t| Array Int (CrossingState $(varT crossType)) |]
        ] ++ map return knotFields] []

    do
        let name = mkName $ "empty" ++ nameBase knotTN

        declare $ do
            ct <- newName "ct"
            sigD name $ forallT [PlainTV ct] (cxt []) [t| $(conT knotTN) $(varT ct) |]

        declare $ funD name $ (:[]) $
            clause [] (normalB $ recConE knotCN $
                    [ (,) vertexCount `fmap` [| 0 :: Int |]
                    , (,) connsArray  `fmap` [| listArray (0 :: Int, -1 :: Int) [] |]
                    , (,) stateArray  `fmap` [| listArray (0 :: Int, -1 :: Int) [] |]
                    , (,) loopsCount  `fmap` [| 0 :: Int |]
                    ] ++ emptyExtraInitializers inst
                ) []

    do
        let name = mkName "changeNumberOfFreeLoops"

        declare $ do
            ct <- newName "ct"
            sigD name $ forallT [PlainTV ct] (cxt []) [t| $(conT knotTN) $(varT ct) -> Int -> $(conT knotTN) $(varT ct) |]

        declare $ funD name $ (:[]) $ do
            loops <- newName "loops"
            knot <- newName "knot"
            clause [varP knot, varP loops] (normalB
                [|  if $(varE loops) >= (0 :: Int)
                        then $(recUpdE (varE knot) [(,) loopsCount `fmap` varE loops])
                        else error "changeNumberOfFreeLoops: number of free loops %i is negative" $(varE loops)
                |]) []

    declare $ do
        ct <- newName "ct"
        k <- newName "k"
        instanceD (cxt [classP ''NFData [varT ct]]) ([t| NFData |] `appT` (conT knotTN `appT` varT ct))
            [ funD 'rnf $ (:[]) $ clause [varP k] (normalB
                    [| rnf ($(varE stateArray) $(varE k)) `seq` $(varE k) `seq` () |]
                ) []
            ]

    declare $ instanceD (cxt []) ([t| PlanarDiagram |] `appT` conT knotTN) $ execWriter $ do
        tell $ (:[]) $ funD 'numberOfVertices $ (:[]) $
            clause [] (normalB $ varE vertexCount) []

        tell $ (:[]) $ funD 'numberOfEdges $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB $
                maybe id ($ varE k) (modifyNumberOfEdges inst)
                    [| numberOfVertices $(varE k) * 2 :: Int |]
                ) []

        tell $ (:[]) $ funD 'nthVertex $ (:[]) $ do
            k <- newName "k"
            i <- newName "i"
            clause [varP k, varP i] (normalB
                [|  let b = numberOfVertices $(varE k)
                    in if $(varE i) < (1 :: Int) || $(varE i) > b
                        then error $ printf "nthVertex: index %i is out of bounds (1, %i)" $(varE i) b
                        else $(conE vertN) $(varE k) ($(varE i) - 1 :: Int)
                |]) []

        tell $ (:[]) $ funD 'nthDart $ (:[]) $ do
            k <- newName "k"
            i <- newName "i"
            clause [varP k, varP i] (normalB
                [|  let b = 2 * numberOfEdges $(varE k) - 1
                    in if $(varE i) < (0 :: Int) || $(varE i) > b
                        then error $ printf "nthDart: index %i is out of bounds (0, %i)" $(varE i) b
                        else $(conE dartN) $(varE k) $(varE i)
                |]) []

        tell $ (:[]) $ funD 'allVertices $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB [|
                    let n = numberOfVertices $(varE k)
                    in map ($(conE vertN) $(varE k)) [0 .. n - 1]
                |]) []

        tell $ (:[]) $ funD 'allHalfEdges $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB [|
                    map ($(conE dartN) $(varE k)) [0 :: Int .. snd $ bounds $ $(varE connsArray) $(varE k)]
                |]) []

        tell $ (:[]) $ funD 'allEdges $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB [|
                    foldl' (\ !es !i ->
                            let j = $(varE connsArray) $(varE k) `unsafeAt` i
                            in if i < j
                                then ($(conE dartN) $(varE k) i, $(conE dartN) $(varE k) j) : es
                                else es
                        ) [] [0 .. snd $ bounds $ $(varE connsArray) $(varE k)]
                |]) []

        tell $ (:[]) $ do
            ct <- newName "ct"
            dataInstD (cxt []) ''Vertex [conT knotTN, varT ct] [normalC vertN
                [ (,) IsStrict `fmap` [t| $(conT knotTN) $(varT ct) |]
                , (,) Unpacked `fmap` [t| Int |]
                ]] []

        tell $ (:[]) $ funD 'vertexDegree $ (:[]) $
            clause [] (normalB [| const (4 :: Int) |]) []

        tell $ (:[]) $ funD 'vertexOwner $ (:[]) $ do
            k <- newName "k"
            clause [conP vertN [varP k, wildP]] (normalB $ varE k) []

        tell $ (:[]) $ funD 'vertexIndex $ (:[]) $ do
            c <- newName "c"
            clause [conP vertN [wildP, varP c]] (normalB [| $(varE c) + 1 :: Int |]) []

        tell $ (:[]) $ funD 'nthOutcomingDart $ (:[]) $ do
            k <- newName "k"
            c <- newName "c"
            i <- newName "i"
            clause [conP vertN [varP k, varP c], varP i] (normalB
                    [| $(conE dartN) $(varE k) (($(varE c) `shiftL` 2) + ($(varE i) .&. 3) :: Int) |]
                ) []

        tell $ (:[]) $ funD 'outcomingDarts $ (:[]) $ do
            c <- newName "c"
            clause [varP c] (normalB [|
                    map (nthOutcomingDart $(varE c)) [0 .. 3]
                |]) []

        tell $ (:[]) $ do
            ct <- newName "ct"
            dataInstD (cxt []) ''Dart [conT knotTN, varT ct] [normalC dartN
                [ (,) IsStrict `fmap` [t| $(conT knotTN) $(varT ct) |]
                , (,) Unpacked `fmap` [t| Int |]
                ]] []

        tell $ (:[]) $ funD 'dartOwner $ (:[]) $ do
            k <- newName "k"
            clause [conP dartN [varP k, wildP]] (normalB $ varE k) []

        tell $ (:[]) $ funD 'dartIndex $ (:[]) $ do
            d <- newName "d"
            clause [conP dartN [wildP, varP d]] (normalB $ varE d) []

        tell $ (:[]) $ funD 'beginVertex $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            clause [conP dartN [varP k, varP d]] (normalB $
                    maybe id ($ (varE k, varE d)) (modifyIncidentCrossing inst)
                        [| $(conE vertN) $(varE k) ($(varE d) `shiftR` 2) |]
                ) []

        tell $ (:[]) $ funD 'beginPlace $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            let patK = maybe wildP (const $ varP k) (modifyDartPlace inst)
            clause [conP dartN [patK, varP d]] (normalB $
                    maybe id ($ (varE k, varE d)) (modifyDartPlace inst)
                        [| $(varE d) .&. 3 :: Int |]
                ) []

        tell $ (:[]) $ funD 'beginPair' $ (:[]) $ do
                d <- newName "d"
                clause [varP d]
                    (guardedB $
                        map (uncurry normalGE) $ map ($ varE d) (extraExplodePairCases ies) ++
                            [ ([| otherwise |], [| first vertexIndex $ beginPair $(varE d) |]) ]
                    ) []

        tell $ (:[]) $ funD 'opposite $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            clause [conP dartN [varP k, varP d]] (normalB
                    [| $(conE dartN) $(varE k) ($(varE connsArray) $(varE k) `unsafeAt` $(varE d)) |]
                ) []

        tell $ (:[]) $ funD 'nextCCW $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            clause [conP dartN [varP k, varP d]] (normalB $
                    maybe id ($ (varE k, varE d)) (modifyNextCCW inst)
                        [| $(conE dartN) $(varE k) (($(varE d) .&. complement 3) + (($(varE d) + 1) .&. 3) :: Int) |]
                ) []

        tell $ (:[]) $ funD 'nextCW $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            clause [conP dartN [varP k, varP d]] (normalB $
                    maybe id ($ (varE k, varE d)) (modifyNextCW inst)
                        [| $(conE dartN) $(varE k) (($(varE d) .&. complement 3) + (($(varE d) - 1) .&. 3) :: Int) |]
                ) []

        tell $ (:[]) $ funD 'isDart $ (:[]) $ do
            k <- newName "k"
            d <- newName "d"
            clause [ maybe wildP (const $ conP dartN [varP k, varP d]) (modifyIsDart inst) ] (normalB $
                    maybe [| True |] ($ (varE k, varE d)) (modifyIsDart inst)
                ) []

        tell $ (:[]) $ funD 'vertexIndicesRange $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB [|
                    (1 :: Int, numberOfVertices $(varE k))
                |]) []

        tell $ (:[]) $ funD 'dartIndicesRange $ (:[]) $ do
            k <- newName "k"
            clause [varP k] (normalB [|
                    (0 :: Int, numberOfDarts $(varE k) - 1 :: Int)
                |]) []

    declare $ instanceD (cxt []) ([t| Knotted |] `appT` conT knotTN) $ execWriter $ do

        tell $ (:[]) $ funD 'numberOfFreeLoops $ (:[]) $
            clause [] (normalB $ varE loopsCount) []

        tell $ (:[]) $ funD 'mapCrossings $ (:[]) $ do
            f <- newName "f"
            k <- newName "k"
            clause [varP f, varP k] (normalB $ recUpdE (varE k) [(,) stateArray `fmap`
                [| amap $(varE f) ($(varE stateArray) $(varE k)) |]]) []

        tell $ (:[]) $ funD 'crossingState $ (:[]) $ do
            k <- newName "k"
            c <- newName "c"
            clause [conP vertN [varP k, varP c]]
                (normalB [| $(varE stateArray) $(varE k) `unsafeAt` $(varE c) |]) []

        tell $ (:[]) $ funD 'forMIncidentDarts $ (:[]) $ do
            k <- newName "k"
            c <- newName "c"
            f <- newName "f"
            clause [conP vertN [varP k, varP c], varP f] (normalB [|
                    let b = $(varE c) `shiftL` 2
                    in $(varE f) ($(conE dartN) $(varE k) b) >> $(varE f) ($(conE dartN) $(varE k) $! b + 1)
                        >> $(varE f) ($(conE dartN) $(varE k) $! b + 2) >> $(varE f) ($(conE dartN) $(varE k) $! b + 3)
                |]) []

        tell $ (:[]) $ funD 'foldMIncidentDarts $ (:[]) $ do
            k <- newName "k"
            c <- newName "c"
            f <- newName "f"
            s <- newName "s"
            clause [conP vertN [varP k, varP c], varP f, varP s] (normalB [|
                    let b = $(varE c) `shiftL` 2
                    in $(varE f) ($(conE dartN) $(varE k) b) $(varE s)
                        >>= $(varE f) ($(conE dartN) $(varE k) $! b + 1)
                            >>= $(varE f) ($(conE dartN) $(varE k) $! b + 2)
                                >>= $(varE f) ($(conE dartN) $(varE k) $! b + 3)
                |]) []

        tell $ (:[]) $ funD 'foldMIncidentDartsFrom $ (:[]) $ do
            dart <- newName "dart"
            k <- newName "k"
            i <- newName "i"
            dir <- newName "dir"
            f <- newName "f"
            s <- newName "s"
            clause [asP dart $ conP dartN [varP k, varP i], bangP $ varP dir, varP f, bangP $ varP s] (normalB $
                maybe id ($ (varE dart, (varE k, varE i))) (modifyFoldMIncidentDartsFrom inst)
                    [|  let d = R.directionSign $(varE dir)
                            r = $(varE i) .&. complement 3
                            l1 = ($(varE i) + d) .&. 3 ; l2 = (l1 + d) .&. 3 ; l3 = (l2 + d) .&. 3
                        in $(varE f) $(varE dart) $(varE s)
                            >>= $(varE f) ($(conE dartN) $(varE k) $! r + l1)
                                >>= $(varE f) ($(conE dartN) $(varE k) $! r + l2)
                                    >>= $(varE f) ($(conE dartN) $(varE k) $! r + l3)
                    |]
                ) []

        do
            tell $ (:[]) $ do
                ct <- newName "ct"
                tySynInstD ''ExplodeType [conT knotTN, varT ct] $ do    
                    let types = [ [t| Int |] ] ++ map (\ (_, x, _) -> x) (extraImplodeExplodeParams ies) ++ [ [t| [([(Int, Int)], CrossingState $(varT ct))] |] ]
                    foldl appT (tupleT $ length types) types

            tell $ (:[]) $ funD 'explode $ (:[]) $ do
                knot <- newName "knot"
                clause [varP knot] (normalB $ tupE $ execWriter $ do
                        tell $ (:[]) $ [| numberOfFreeLoops $(varE knot) |]
                        tell $ map (\ (_, _, f) -> f $ varE knot) $ extraImplodeExplodeParams ies
                        tell $ (:[]) [| map (\ c -> (map endPair' $ outcomingDarts c, crossingState c)) $ allVertices $(varE knot) |]
                    ) []

            tell $ (:[]) $ funD 'implode $ (:[]) $ do
                    arg <- newName "arg"
                    loops <- newName "loops"
                    list <- newName "list"
                    n <- newName "n"
                    cr <- newName "cr"
                    st <- newName "st"
                    cr' <- newName "cr'"
                    st' <- newName "st'"

                    clause [asP arg $ tupP $ map (bangP . varP) $ [loops] ++ (map (\ (x, _, _) -> x) $ extraImplodeExplodeParams ies) ++ [list]] (normalB $
                        appE (varE 'runST) $ doE $ execWriter $ do
                            let spliceError text args =
                                    let literal = litE $ stringL $ nameBase knotTN ++ ".implode: " ++ text ++ " at %s"
                                    in appE [| error |] $ foldl appE [| printf $literal |] $ args ++ [ [| show $(varE arg) |] ]

                            tell $ (:[]) $ noBindS
                                [|  when ($(varE loops) < (0 :: Int)) $
                                        $(spliceError "number of free loops %i is negative" [varE loops])
                                |]

                            tell $ implodePreExtra ies spliceError

                            tell $ (:[]) $ letS $ (:[]) $ valD (varP n) (normalB [| length $(varE list) |]) []
                            tell $ (:[]) $ bindS (varP cr)
                                [|  let border = $(maybe id ($ varE n) (modifyImplodeLimit ies) [| 4 * $(varE n) - 1 :: Int |]) 
                                    in newArray_ (0, border) :: ST s (STUArray s Int Int)
                                |]

                            tell $ (:[]) $ bindS (varP st) [| newArray_ (0, $(varE n) - 1) :: ST s (STArray s Int a) |]

                            let spliceFill a (c, p) =
                                    let guards = map (\ f -> f spliceError (varE n) c p) (extraImplodePairCases ies) ++
                                            [ ([| $c < (1 :: Int) || $c > $(varE n)  |], spliceError "crossing index %i is out of bounds [1 .. %i]" [c, varE n])
                                            , ([| $p < (0 :: Int) || $p > (3 :: Int) |], spliceError "place index %i is out of bounds [0 .. 3]" [p])
                                            , ([| otherwise                          |], [| 4 * ($c - 1) + $p :: Int |])
                                            ]
                                    in [| do
                                            let b = $(caseE [| () |] [match wildP (guardedB $ map (uncurry normalGE) guards) []])
                                            when ($a == b) $
                                                $(spliceError "(%i, %i) connected to itself" [c, p])

                                            unsafeWrite $(varE cr) $a b
                                            when (b < $a) $ do
                                                x <- unsafeRead $(varE cr) b
                                                when (x /= $a) $
                                                    $(spliceError "(%i, %i) points to unconsistent position" [c, p])
                                    |]

                            tell $ (:[]) $ noBindS
                                [|  forM_ (zip $(varE list) [0 ..]) $ \ ((!ns, !cs), !i) -> do
                                        unsafeWrite $(varE st) i cs
                                        case ns of
                                            [p0, p1, p2, p3] ->
                                                forM_ [(p0, 0), (p1, 1), (p2, 2), (p3, 3)] $ \ ((!c, !p), !j) ->
                                                    let a = 4 * i + j
                                                    in $(spliceFill [| a |] ([| c |], [| p |]))
                                            _                ->
                                                $(spliceError "there must be 4 neighbours for every crossing, but found %i for %i-th" [
                                                    [| length ns |], [| i + 1 :: Int |] ])
                                |]

                            tell $ implodePostExtra ies (varE n) (varE cr) spliceFill

                            tell $ (:[]) $ bindS (varP cr') [| unsafeFreeze $(varE cr) |]
                            tell $ (:[]) $ bindS (varP st') [| unsafeFreeze $(varE st) |]
                            tell $ (:[]) $ noBindS
                                [|  return $! $(recConE knotCN $
                                        [ (,) vertexCount `fmap` varE n
                                        , (,) connsArray  `fmap` varE cr'
                                        , (,) stateArray  `fmap` varE st'
                                        , (,) loopsCount  `fmap` varE loops
                                        ] ++ implodeInitializers ies)
                                |]
                        ) []


    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Eq |] $ conT vertN `appT` conT knotTN `appT` varT ct)
            [ funD '(==) $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [conP vertN [wildP, varP a], conP vertN [wildP, varP b]]
                    (normalB [| $(varE a) == $(varE b) |]) []
            ]

    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Ord |] $ conT vertN `appT` conT knotTN `appT` varT ct)
            [ funD 'compare $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [conP vertN [wildP, varP a], conP vertN [wildP, varP b]]
                    (normalB [| $(varE a) `compare` $(varE b) |]) []
            ]

    declare $ do
        ct <- newName "ct"
        instanceD (cxt [classP ''NFData [varT ct]]) (appT [t| NFData |] $ conT vertN `appT` conT knotTN `appT` varT ct) []


    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Eq |] $ conT dartN `appT` conT knotTN `appT` varT ct)
            [ funD '(==) $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]]
                    (normalB [| $(varE a) == $(varE b) |]) []
            ]

    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Ord |] $ conT dartN `appT` conT knotTN `appT` varT ct)
            [ funD 'compare $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]]
                    (normalB [| $(varE a) `compare` $(varE b) |]) []
            ]

    declare $ do
        ct <- newName "ct"
        instanceD (cxt [classP ''NFData [varT ct]]) (appT [t| NFData |] $ conT dartN `appT` conT knotTN `appT` varT ct) []


    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Ix |] $ conT vertN `appT` conT knotTN `appT` varT ct)
            [ funD 'range $ (:[]) $ do
                k <- newName "k"
                a <- newName "a"
                b <- newName "b"
                clause [tupP [conP vertN [wildP, varP a], conP vertN [varP k, varP b]]]
                    (normalB [| map ($(conE vertN) $(varE k)) [$(varE a) .. $(varE b)] |]) []

            , funD 'rangeSize $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [tupP [conP vertN [wildP, varP a], conP vertN [wildP, varP b]]]
                    (normalB [| max 0 $ 1 + $(varE b) - $(varE a) :: Int |]) []

            , funD 'inRange $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                i <- newName "i"
                clause [tupP [conP vertN [wildP, varP a], conP vertN [wildP, varP b]], conP vertN [wildP, varP i]]
                    (normalB [| ($(varE i) >= $(varE a)) && ($(varE i) <= $(varE b)) |]) []

            , funD 'index $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                i <- newName "i"
                clause [tupP [conP vertN [wildP, varP a], conP vertN [wildP, varP b]], conP vertN [wildP, varP i]]
                    (guardedB $ map (uncurry normalGE)
                        [ ([| ($(varE i) >= $(varE a)) && ($(varE i) <= $(varE b)) |], [| $(varE i) - $(varE a) :: Int |])
                        , ([| otherwise |], [| error "out of range" |])
                        ]) []
            ]


    declare $ do
        ct <- newName "ct"
        instanceD (cxt []) (appT [t| Ix |] $ conT dartN `appT` conT knotTN `appT` varT ct)
            [ funD 'range $ (:[]) $ do
                k <- newName "k"
                a <- newName "a"
                b <- newName "b"
                clause [tupP [conP dartN [wildP, varP a], conP dartN [varP k, varP b]]]
                    (normalB [| map ($(conE dartN) $(varE k)) [$(varE a) .. $(varE b)] |]) []

            , funD 'rangeSize $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                clause [tupP [conP dartN [wildP, varP a], conP dartN [wildP, varP b]]]
                    (normalB [| max 0 $ 1 + $(varE b) - $(varE a) :: Int |]) []

            , funD 'inRange $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                i <- newName "i"
                clause [tupP [conP dartN [wildP, varP a], conP dartN [wildP, varP b]], conP dartN [wildP, varP i]]
                    (normalB [| ($(varE i) >= $(varE a)) && ($(varE i) <= $(varE b)) |]) []

            , funD 'index $ (:[]) $ do
                a <- newName "a"
                b <- newName "b"
                i <- newName "i"
                clause [tupP [conP dartN [wildP, varP a], conP dartN [wildP, varP b]], conP dartN [wildP, varP i]]
                    (guardedB $ map (uncurry normalGE)
                        [ ([| ($(varE i) >= $(varE a)) && ($(varE i) <= $(varE b)) |], [| $(varE i) - $(varE a) :: Int |])
                        , ([| otherwise |], [| error "out of range" |])
                        ]) []
            ]
