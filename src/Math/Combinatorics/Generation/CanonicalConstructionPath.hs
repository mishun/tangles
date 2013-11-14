{-# LANGUAGE ExistentialQuantification #-}
module Math.Combinatorics.Generation.CanonicalConstructionPath
    ( CanonicalConstructionPath(..)
    , forCCPFrom_
    , forCCP_
    , CanonicalConstructionPathI(..)
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (mfilter, guard, (>=>))


class CanonicalConstructionPath p where
    filterBranch :: (x -> Bool) -> p x u l -> p x u l
    filterUpper  :: (x -> u -> Bool) -> p x u l -> p x u l
    filterLower  :: (u -> l -> Bool) -> p x u l -> p x u l
    substLower   :: (l -> Maybe l') -> (l' -> l) -> p x u l -> p x u l'
    withRoots    :: p x u l -> [x] -> p x u l

    rootsList         :: p x u l -> [x]
    canonicalChildren :: p x u l -> x -> [x]


forCCPFrom_ :: (Monad m, CanonicalConstructionPath p) => p x u l -> (x -> m ()) -> x -> m ()
forCCPFrom_ c yield =
    let scan x = yield x >> mapM_ scan (canonicalChildren c x)
    in scan


forCCP_ :: (Monad m, CanonicalConstructionPath p) => p x u l -> (x -> m ()) -> m ()
forCCP_ c yield =
    mapM_ (forCCPFrom_ c yield) (rootsList c)


data CanonicalConstructionPathI x u l =
    CanonicalConstructionPathClean
        { independentUpper :: x -> [u]
        , tryAscent        :: u -> Maybe l
        , lowerProjection  :: l -> x
        , roots            :: [x]
        }
    | forall ord. (Ord ord) => CanonicalConstructionPathFiltering
        { independentUpper :: x -> [u]
        , tryAscent        :: u -> Maybe l
        , lowerProjection  :: l -> x
        , roots            :: [x]
        , filterInvariant  :: l -> ord
        }


instance CanonicalConstructionPath CanonicalConstructionPathI where
    filterBranch f c =
        c { independentUpper = \ x -> guard (f x) >> independentUpper c x } 

    filterUpper f c =
        c { independentUpper = \ x -> filter (f x) (independentUpper c x) }

    filterLower f c =
        c { tryAscent = \ u -> mfilter (f u) (tryAscent c u) }

    substLower try' proj c =
        case c of
            CanonicalConstructionPathClean {} ->
                CanonicalConstructionPathClean
                    { independentUpper = independentUpper c
                    , tryAscent        = tryAscent c >=> try'
                    , lowerProjection  = lowerProjection c . proj
                    , roots            = roots c 
                    }
            CanonicalConstructionPathFiltering { filterInvariant = x } ->
                CanonicalConstructionPathFiltering
                    { independentUpper = independentUpper c
                    , tryAscent        = tryAscent c >=> try'
                    , lowerProjection  = lowerProjection c . proj
                    , filterInvariant  = x . proj
                    , roots            = roots c 
                    }

    withRoots c r = c { roots = r }

    rootsList = roots

    canonicalChildren c =
        map (lowerProjection c) . mapMaybe (tryAscent c) . independentUpper c
