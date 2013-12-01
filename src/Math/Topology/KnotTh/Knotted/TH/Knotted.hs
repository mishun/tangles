{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.Topology.KnotTh.Knotted.TH.Knotted
    ( KnottedSettings(..)
    , ImplodeExplodeSettings(..)
    , defaultKnotted
    , defaultImplodeExplode
    , produceKnotted
    ) where

import Language.Haskell.TH
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
import Control.Monad.Writer (MonadWriter, WriterT, execWriter, execWriterT, tell)
import Control.Monad ((>=>), when, forM_)
import Control.Monad.Trans (lift)
import Control.DeepSeq
import Text.Printf
import Internal.Macro
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


append :: (MonadWriter [a] m) => a -> m ()
append = tell . (: [])


append' :: Q Dec -> WriterT [Dec] Q ()
append' = lift >=> tell . (:[])


appends' :: Q [Dec] -> WriterT [Dec] Q ()
appends' = lift >=> tell


appendF :: (MonadWriter [Q Dec] m) => Name -> Q Clause -> m ()
appendF name body = tell $ (:[]) $ funD name [body]


loopsCount, vertexCount, crossingsArray, involutionArray :: Name
loopsCount = mkName "loopsCount"
vertexCount = mkName "vertexCount"
crossingsArray = mkName "crossingsArray"
involutionArray = mkName "involutionArray"


produceKnotted :: Q [Dec] -> KnottedSettings -> Q [Dec]
produceKnotted knotPattern inst = do
    declarations@(DataD [] knotType [PlainTV crossType] [RecC knotConstr knotFields] [] : _) <- knotPattern

    artificial <- execWriterT $ do
        append' $ dataD (cxt []) knotType [PlainTV crossType]
            [ recC knotConstr $
                [ (,,) loopsCount Unpacked `fmap` [t| Int |]
                , (,,) vertexCount Unpacked `fmap` [t| Int |]
                , (,,) involutionArray Unpacked `fmap` [t| UArray Int Int |]
                , (,,) crossingsArray Unpacked `fmap` [t| Array Int $(varT crossType) |]
                ]
            ] []

        let ies = implodeExplodeSettings inst

        appends'
            [d| instance (NFData a) => NFData ($(conT knotType) a) where
                    rnf k = rnf ($(varE crossingsArray) k) `seq` k `seq` ()

                instance (NFData a) => NFData (Vertex $(conT knotType) a)

                instance (NFData a) => NFData (Dart $(conT knotType) a)

                instance Functor $(conT knotType) where
                    fmap f k = $(recUpdE [| k |] [(,) crossingsArray `fmap` [| amap f ($(varE crossingsArray) k) |] ])

                instance PlanarDiagram $(conT knotType) where
                    numberOfVertices = $(varE vertexCount)

                    numberOfEdges k = $(maybe id ($ [| k |]) (modifyNumberOfEdges inst) [| numberOfVertices k * 2 |])

                    nthVertex k i =
                        let b = numberOfVertices k
                        in if i < 1 || i > b
                            then error $ printf "nthVertex: index %i is out of bounds (1, %i)" i b
                            else Vertex k (i - 1)

                    nthDart k i =
                        let b = 2 * numberOfEdges k - 1
                        in if i < 0 || i > b
                            then error $ printf "nthDart: index %i is out of bounds (0, %i)" i b
                            else Dart k i

                    allVertices k =
                        let n = numberOfVertices k
                        in map (Vertex k) [0 .. n - 1]

                    allHalfEdges k =
                        map (Dart k) [0 :: Int .. snd $ bounds $ $(varE involutionArray) k]

                    allEdges k =
                        foldl' (\ !es !i ->
                                let j = $(varE involutionArray) k `unsafeAt` i
                                in if i < j
                                    then (Dart k i, Dart k j) : es
                                    else es
                            ) [] [0 .. snd $ bounds $ $(varE involutionArray) k]

                    data Vertex $(conT knotType) a = Vertex !($(conT knotType) a) {-# UNPACK #-} !Int

                    vertexDegree _ = 4
                    vertexOwner (Vertex k _) = k
                    vertexIndex (Vertex _ i) = i + 1

                    nthOutcomingDart (Vertex k c) i = Dart k ((c `shiftL` 2) + (i .&. 3))

                    outcomingDarts c = map (nthOutcomingDart c) [0 .. 3]

                    data Dart $(conT knotType) a = Dart !($(conT knotType) a) {-# UNPACK #-} !Int

                    dartOwner (Dart k _) = k
                    dartIndex (Dart _ i) = i

                    opposite (Dart k d) = Dart k ($(varE involutionArray) k `unsafeAt` d)

                    beginVertex (Dart k d) =
                        $(maybe id ($ ([| k |], [| d |])) (modifyIncidentCrossing inst)
                            [| Vertex k (d `shiftR` 2) |]
                        )

                    beginPlace (Dart k d) =
                        $(maybe id ($ ([| k |], [| d |])) (modifyDartPlace inst)
                            [| d .&. 3 |]
                        )

                    nextCCW (Dart k d) =
                        $(maybe id ($ ([| k |], [| d |])) (modifyNextCCW inst)
                            [| Dart k ((d .&. complement 3) + ((d + 1) .&. 3)) |]
                        )

                    nextCW (Dart k d) =
                        $(maybe id ($ ([| k |], [| d |])) (modifyNextCW inst)
                            [| Dart k ((d .&. complement 3) + ((d - 1) .&. 3)) |]
                        )

                    isDart (Dart k d) = $(maybe [| True |] ($ ([| k |], [| d |])) (modifyIsDart inst))

                    vertexIndicesRange k = (1, numberOfVertices k)

                    dartIndicesRange k = (0, numberOfDarts k - 1)

                instance Knotted $(conT knotType) where
                    vertexCrossing (Vertex k c) = $(varE crossingsArray) k `unsafeAt` c

                    numberOfFreeLoops = $(varE loopsCount)

                    changeNumberOfFreeLoops loops knot =
                        if loops >= 0
                            then $(recUpdE [| knot |] [(,) loopsCount `fmap` [| loops |] ])
                            else error "changeNumberOfFreeLoops: number of free loops %i is negative" loops

                    emptyKnotted =
                        $(recConE knotConstr $
                            [ (,) vertexCount `fmap` [| 0 :: Int |]
                            , (,) involutionArray `fmap` [| listArray (0 :: Int, -1 :: Int) [] |]
                            , (,) crossingsArray `fmap` [| listArray (0 :: Int, -1 :: Int) [] |]
                            , (,) loopsCount `fmap` [| 0 :: Int |]
                            ] ++ emptyExtraInitializers inst
                        )

                    homeomorphismInvariant = undefined
                    isConnected = undefined

                    type ExplodeType $(conT knotType) a = (Int, [([(Int, Int)], a)])
                    explode = undefined
                    implode = undefined

            |]

        append' $ instanceD (cxt []) ([t| PlanarDiagram |] `appT` conT knotType) $ execWriter $ do
            appendF 'beginPair' $ do
                d <- newName "d"
                clause [varP d]
                    (guardedB $
                        map (uncurry normalGE) $ map ($ varE d) (extraExplodePairCases ies) ++
                            [ ([| otherwise |], [| first vertexIndex $ beginPair $(varE d) |]) ]
                    ) []

        append' $ instanceD (cxt []) ([t| Knotted |] `appT` conT knotType) $ execWriter $ do
            appendF 'implode $ do
                loops <- newName "loops"
                list <- newName "list"
                n <- newName "n"
                cr <- newName "cr"
                st <- newName "st"
                cr' <- newName "cr'"
                st' <- newName "st'"

                clause [tupP $ map (bangP . varP) $ [loops] ++ (map (\ (x, _, _) -> x) $ extraImplodeExplodeParams ies) ++ [list]] (normalB $
                        appE (varE 'runST) $ doE $ execWriter $ do
                            let spliceError text args =
                                    let literal = litE $ stringL $ nameBase knotType ++ ".implode: " ++ text
                                    in appE [| error |] $ foldl appE [| printf $literal |] args

                            append $ noBindS
                                [|  when ($(varE loops) < (0 :: Int)) $
                                        $(spliceError "number of free loops %i is negative" [varE loops])
                                |]

                            tell $ implodePreExtra ies spliceError

                            append $ letS $ (:[]) $ valD (varP n) (normalB [| length $(varE list) |]) []
                            append $ bindS (varP cr)
                                [|  let border = $(maybe id ($ varE n) (modifyImplodeLimit ies) [| 4 * $(varE n) - 1 :: Int |]) 
                                    in newArray_ (0, border) :: ST s (STUArray s Int Int)
                                |]

                            append $ bindS (varP st) [| newArray_ (0, $(varE n) - 1) :: ST s (STArray s Int a) |]

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

                            append $ noBindS
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

                            append $ bindS (varP cr') [| unsafeFreeze $(varE cr) |]
                            append $ bindS (varP st') [| unsafeFreeze $(varE st) |]
                            append $ noBindS
                                [|  return $! $(recConE knotConstr $
                                        [ (,) vertexCount `fmap` varE n
                                        , (,) involutionArray `fmap` varE cr'
                                        , (,) crossingsArray `fmap` varE st'
                                        , (,) loopsCount `fmap` varE loops
                                        ] ++ implodeInitializers ies)
                                |]
                    ) []

    mergeDeclarations (artificial ++ declarations)
