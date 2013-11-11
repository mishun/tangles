{-# LANGUAGE ExistentialQuantification #-}
module Math.Combinatorics.Generation.CanonicalConstructionPath
    ( CanonicalConstructionPath(..)
    , filterUpper
    , filterLower
    , substLower
    , canonicalChildren
    , forCCPFrom_
    , RootedCanonicalConstructionPath
    , withRoots
    , forCCP_
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (mfilter, (>=>))


data CanonicalConstructionPath x u l =
    CanonicalConstructionPathClean
        { independentUpper :: x -> [u]
        , tryAscent        :: u -> Maybe l
        , lowerProjection  :: l -> x
        }
    | forall ord. (Ord ord) => CanonicalConstructionPathFiltering
        { independentUpper :: x -> [u]
        , tryAscent        :: u -> Maybe l
        , lowerProjection  :: l -> x
        , filterInvariant  :: l -> ord
        }


filterUpper :: (u -> Bool) -> CanonicalConstructionPath x u l -> CanonicalConstructionPath x u l
filterUpper f c = c { independentUpper = filter f . independentUpper c }


filterLower :: (l -> Bool) -> CanonicalConstructionPath x u l -> CanonicalConstructionPath x u l
filterLower f c = c { tryAscent = mfilter f . tryAscent c }


substLower :: (l -> Maybe l') -> (l' -> l) -> CanonicalConstructionPath x u l -> CanonicalConstructionPath x u l'
substLower try' proj c =
    case c of
        CanonicalConstructionPathClean {} ->
            CanonicalConstructionPathClean
                { independentUpper = independentUpper c
                , tryAscent        = tryAscent c >=> try'
                , lowerProjection  = lowerProjection c . proj 
                }
        CanonicalConstructionPathFiltering { filterInvariant = x} ->
            CanonicalConstructionPathFiltering
                { independentUpper = independentUpper c
                , tryAscent        = tryAscent c >=> try'
                , lowerProjection  = lowerProjection c . proj
                , filterInvariant  = x . proj 
                }


canonicalChildren :: CanonicalConstructionPath x u l -> x -> [x]
canonicalChildren c =
    map (lowerProjection c) . mapMaybe (tryAscent c) . independentUpper c


forCCPFrom_ :: (Monad m) => CanonicalConstructionPath x u l -> (x -> m ()) -> x -> m ()
forCCPFrom_ c yield =
    let scan x = yield x >> mapM_ scan (canonicalChildren c x)
    in scan


data RootedCanonicalConstructionPath x =
    forall u l. RootedCanonicalConstructionPath (CanonicalConstructionPath x u l) [x]


withRoots :: CanonicalConstructionPath x u l -> [x] -> RootedCanonicalConstructionPath x
withRoots = RootedCanonicalConstructionPath


forCCP_ :: (Monad m) => RootedCanonicalConstructionPath x -> (x -> m ()) -> m ()
forCCP_ (RootedCanonicalConstructionPath path roots) yield =
    mapM_ (forCCPFrom_ path yield) roots
