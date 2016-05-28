{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Math.Topology.KnotTh.Enumeration.EquivalenceClasses
    ( equivalenceClasses
    ) where

import Control.Monad (when, unless, forM_)
import qualified Control.Monad.State.Strict as State
import Data.Function (fix)
import qualified Data.IntDisjointSet as DS
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as Map
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Enumeration.DiagramInfo


data State k v =
    St  { set  :: !DS.IntDisjointSet
        , keys :: !(Map.Map k Int)
        , vals :: !(IM.IntMap v)
        }


equivalenceClasses
    :: (KnotWithPrimeTest k a, DiagramInfo info)
        => [k a -> [k a]] -> (forall m. (Monad m) => (k a -> m ()) -> m ()) -> [info (k a)]

equivalenceClasses moves enumerateDiagrams =
    IM.elems $ vals $ flip State.execState St {set = DS.empty, keys = Map.empty, vals = IM.empty} $ do
        let declareEquivalent !a !b = do
                eq <- do
                    !st <- State.get
                    let ai = keys st Map.! a
                    let bi = keys st Map.! b
                    let (result, nextDS) = DS.equivalent ai bi $ set st
                    State.put $! st { set = nextDS }
                    return $! result
                unless eq $ do
                    !st <- State.get
                    let ds0 = set st
                    let (Just !ai, !ds1) = DS.lookup (keys st Map.! a) ds0
                    let (Just !bi, !ds2) = DS.lookup (keys st Map.! b) ds1
                    let !ds3 = DS.union ai bi ds2
                    let (Just !resi, !ds4) = DS.lookup ai ds3
                    let !result = merge (vals st IM.! bi) $ vals st IM.! ai
                    State.put $! st { set = ds4, vals = IM.insert resi result $ IM.delete bi $ IM.delete ai $ vals st }

        let insert !code !current = do
                !st <- State.get
                let ds = set st
                case Map.lookup code $ keys st of
                    Nothing   -> do
                        let !rootId = DS.size ds
                        State.put $! st
                            { set  = DS.insert rootId ds
                            , keys = Map.insert code rootId $ keys st
                            , vals = IM.insert rootId current $ vals st
                            }
                        return True
                    Just elId -> do
                        let (Just rootId, ds') = DS.lookup elId ds
                        State.put $! st { set = ds', vals = IM.insertWith' merge rootId current $ vals st }
                        return False

        enumerateDiagrams $
            flip fix Nothing $ \ dfs !prevCode !diagram -> do
                let code = unrootedHomeomorphismInvariant diagram
                inserted <- insert code $ wrap diagram
                maybe (return ()) (declareEquivalent code) prevCode
                when inserted $
                    forM_ (concatMap ($ diagram) moves) (dfs $ Just code)
