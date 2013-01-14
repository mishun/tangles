{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Knotted.TH.Knotted
	( KnottedSettings(..)
	, ImplodeExplodeSettings(..)
	, defaultKnotted
	, defaultImplodeExplode
	, produceKnotted
	) where

import Language.Haskell.TH
import Data.List (foldl')
import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.Array.Base (bounds, unsafeAt, unsafeWrite, unsafeRead)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STArray, STUArray, runSTArray, newArray_)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (execStateT, modify)
import Control.Monad (when, forM_)
import Control.Monad.Trans (lift)
import Control.DeepSeq
import Text.Printf
import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted


data KnottedSettings = KnottedSettings
	{ modifyNumberOfEdges          :: Maybe (ExpQ -> ExpQ -> ExpQ)
	, modifyIsDart                 :: Maybe ((ExpQ, ExpQ) -> ExpQ)
	, modifyNextCCW                :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
	, modifyNextCW                 :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
	, modifyDartPlace              :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
	, modifyIncidentCrossing       :: Maybe ((ExpQ, ExpQ) -> ExpQ -> ExpQ)
	, modifyFoldMIncidentDartsFrom :: Maybe ((ExpQ, (ExpQ, ExpQ)) -> ExpQ -> ExpQ)
	, implodeExplodeSettings       :: Maybe ImplodeExplodeSettings
	}


data ImplodeExplodeSettings = ImplodeExplodeSettings
	{ implodeInitializers :: [Q (Name, Exp)]
	, implodeExtra        :: [StmtQ]
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
	, implodeExplodeSettings       = Just defaultImplodeExplode
	}


defaultImplodeExplode :: ImplodeExplodeSettings
defaultImplodeExplode = ImplodeExplodeSettings
	{ implodeInitializers = []
	, implodeExtra        = []
	}


produceKnotted :: DecsQ -> KnottedSettings -> DecsQ
produceKnotted knotPattern inst = flip execStateT [] $ do
	let declare d = lift d >>= \ d' -> modify (++ [ d' ])
	let maybeM x f = case x of
		Nothing -> return ()
		Just jx -> f jx

	[DataD [] knotTN [PlainTV crossType] [RecC knotCN knotFields] []] <- lift knotPattern

	let dartN = mkName "Dart"
	let crosN = mkName "Crossing"
	let dartKnotN = mkName $ "dart" ++ nameBase knotTN
	let crosKnotN = mkName $ "crossing" ++ nameBase knotTN

	let loopsCount = mkName "loopsCount"
	let crossCount = mkName "crossCount"
	let stateArray = mkName "stateArray"
	let crossArray = mkName "crossArray"

	declare $ dataD (cxt []) knotTN [PlainTV crossType] [recC knotCN $ map return (
		[ (loopsCount, Unpacked, ConT ''Int)
		, (crossCount, Unpacked, ConT ''Int)
		, (crossArray, Unpacked, ConT ''UArray `AppT` ConT ''Int `AppT` ConT ''Int)
		, (stateArray, Unpacked, ConT ''Array `AppT` ConT ''Int `AppT` (ConT ''CrossingState `AppT` VarT crossType))
		] ++ knotFields)] []

	declare $ do
		ct <- newName "ct"
		dataD (cxt []) dartN [PlainTV ct] [normalC dartN
			[ (,) IsStrict `fmap` (conT knotTN `appT` varT ct)
			, (,) Unpacked `fmap` conT ''Int
			]] []

	declare $ do
		ct <- newName "ct"
		sigD dartKnotN $ forallT [PlainTV ct] (cxt [])
			[t| $(conT dartN) $(varT ct) -> $(conT knotTN) $(varT ct) |]

	declare $ return $ PragmaD $ InlineP dartKnotN $ InlineSpec True False Nothing

	declare $ funD dartKnotN $ (:[]) $ do
		k <- newName "k"
		clause [conP dartN [varP k, wildP]] (normalB $ varE k) []

	declare $ instanceD (cxt []) (appT [t|Eq|] $ conT dartN `appT` varT (mkName "ct"))
		[ funD '(==) $ (:[]) $ do
			a <- newName "a"
			b <- newName "b"
			clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]] (normalB [|
				$(varE a) == $(varE b)
				|]) []
		]

	declare $ instanceD (cxt []) (appT [t|Ord|] $ conT dartN `appT` varT (mkName "ct"))
		[ funD 'compare $ (:[]) $ do
			a <- newName "a"
			b <- newName "b"
			clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]] (normalB [|
				$(varE a) `compare` $(varE b)
				|]) []
		]

	declare $ do
		ct <- newName "ct"
		dataD (cxt []) crosN [PlainTV ct] [normalC crosN
			[ (,) IsStrict `fmap` (conT knotTN `appT` varT ct)
			, (,) Unpacked `fmap` conT ''Int
			]] []

	declare $ do
		ct <- newName "ct"
		sigD crosKnotN $ forallT [PlainTV ct] (cxt [])
			[t| $(conT crosN) $(varT ct) -> $(conT knotTN) $(varT ct) |]

	declare $ return $ PragmaD $ InlineP crosKnotN $ InlineSpec True False Nothing

	declare $ funD crosKnotN $ (:[]) $ do
		k <- newName "k"
		clause [conP crosN [varP k, wildP]] (normalB $ varE k) []

	declare $ instanceD (cxt []) (appT [t|Eq|] $ conT crosN `appT` varT (mkName "ct"))
		[ funD '(==) $ (:[]) $ do
			a <- newName "a"
			b <- newName "b"
			clause [conP crosN [wildP, varP a], conP crosN [wildP, varP b]] (normalB [|
				$(varE a) == $(varE b)
				|]) []
		]

	declare $ instanceD (cxt []) (appT [t|Ord|] $ conT crosN `appT` varT (mkName "ct"))
		[ funD 'compare $ (:[]) $ do
			a <- newName "a"
			b <- newName "b"
			clause [conP crosN [wildP, varP a], conP crosN [wildP, varP b]] (normalB [|
				$(varE a) `compare` $(varE b)
				|]) []
		]

	declare $ do
		ct <- newName "ct"
		k <- newName "k"
		instanceD (cxt [classP ''NFData [varT ct]]) ([t|NFData|] `appT` (conT knotTN `appT` varT ct))
			[ funD 'rnf $ (:[]) $ clause [varP k] (normalB [|
				rnf ($(varE stateArray) $(varE k)) `seq` $(varE k) `seq` ()
				|]) []
			]

	declare $ instanceD (cxt []) ([t|Knotted|] `appT` conT knotTN `appT` conT crosN `appT` conT dartN)
		[ funD 'numberOfFreeLoops $ (:[]) $ clause [] (normalB $ varE loopsCount) []

		, funD 'numberOfCrossings $ (:[]) $ clause [] (normalB $ varE crossCount) []

		, funD 'numberOfEdges $ (:[]) $ do
			k <- newName "k"
			clause [varP k] (normalB $
				maybe id ($ varE k) (modifyNumberOfEdges inst)
					[| numberOfCrossings $(varE k) * (2 :: Int) |]
				) []

		, funD 'allEdges $ (:[]) $ do
			k <- newName "k"
			clause [varP k] (normalB $ [|
				foldl' (\ !es !i ->
					let j = $(varE crossArray) $(varE k) `unsafeAt` i
					in if i < j
						then ($(conE dartN) $(varE k) i, $(conE dartN) $(varE k) j) : es
						else es
					) [] [0 .. snd $ bounds $ $(varE crossArray) $(varE k)]
				|]) []

		, funD 'crossingOwner $ (:[]) $ do
			k <- newName "k"
			clause [conP crosN [varP k, wildP]] (normalB $ varE k) []

		, funD 'crossingIndex $ (:[]) $ do
			c <- newName "c"
			clause [conP crosN [wildP, varP c]] (normalB [| $(varE c) + (1 :: Int) |]) []

		, funD 'crossingState $ (:[]) $ do
			k <- newName "k"
			c <- newName "c"
			clause [conP crosN [varP k, varP c]] (normalB [|
				$(varE stateArray) $(varE k) `unsafeAt` $(varE c)
				|]) []

		, funD 'nthCrossing $ (:[]) $ do
			k <- newName "k"
			i <- newName "i"
			clause [varP k, varP i] (normalB [|
				if $(varE i) < (1 :: Int) || $(varE i) > numberOfCrossings $(varE k)
					then error $ printf "nthCrossing: index %i is out of bounds (1, %i)" $(varE i) (numberOfCrossings $(varE k))
					else $(conE crosN) $(varE k) ($(varE i) - (1 :: Int))
				|]) []

		, funD 'mapCrossings $ (:[]) $ do
			f <- newName "f"
			k <- newName "k"
			clause [varP f, varP k] (normalB $ recUpdE (varE k) [(,) stateArray `fmap` [|
				runSTArray $ do
					let n = numberOfCrossings $(varE k)
					st <- newArray_ (0, n - 1)
					forM_ [0 .. n - 1] $ \ !i ->
						unsafeWrite st i $! $(varE f) $! $(varE stateArray) $(varE k) `unsafeAt` i
					return $! st
				|]]) []

		, funD 'nthIncidentDart $ (:[]) $ do
			k <- newName "k"
			c <- newName "c"
			i <- newName "i"
			clause [conP crosN [varP k, varP c], varP i] (normalB [|
				$(conE dartN) $(varE k) $! ($(varE c) `shiftL` 2 :: Int) + ($(varE i) .&. 3 :: Int)
				|]) []

		, funD 'isDart $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			clause [ maybe wildP (const $ conP dartN [varP k, varP d]) (modifyIsDart inst) ] (normalB $
				maybe [| True |] ($ (varE k, varE d)) (modifyIsDart inst)
				) []

		, funD 'dartOwner $ (:[]) $ do
			k <- newName "k"
			clause [conP dartN [varP k, wildP]] (normalB $ varE k) []

		, funD 'dartPlace $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			let patK = maybe wildP (const $ varP k) (modifyDartPlace inst)
			clause [conP dartN [patK, varP d]] (normalB $
				maybe id ($ (varE k, varE d)) (modifyDartPlace inst)
					[| $(varE d) .&. (3 :: Int) |]
				) []

		, funD 'incidentCrossing $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			clause [conP dartN [varP k, varP d]] (normalB $
				maybe id ($ (varE k, varE d)) (modifyIncidentCrossing inst)
					[| $(conE crosN) $(varE k) $! $(varE d) `shiftR` (2 :: Int) |]
				) []

		, funD 'opposite $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			clause [conP dartN [varP k, varP d]] (normalB [|
				$(conE dartN) $(varE k) $! $(varE crossArray) $(varE k) `unsafeAt` $(varE d)
				|]) []

		, funD 'nextCCW $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			clause [conP dartN [varP k, varP d]] (normalB $
				maybe id ($ (varE k, varE d)) (modifyNextCCW inst)
					[| $(conE dartN) $(varE k) $! ($(varE d) .&. complement 3 :: Int) + (($(varE d) + 1) .&. 3 :: Int) |]
				) []

		, funD 'nextCW $ (:[]) $ do
			k <- newName "k"
			d <- newName "d"
			clause [conP dartN [varP k, varP d]] (normalB $
				maybe id ($ (varE k, varE d)) (modifyNextCW inst)
					[| $(conE dartN) $(varE k) $! ($(varE d) .&. complement 3 :: Int) + (($(varE d) - 1) .&. 3 :: Int) |]
				) []

		, funD 'dartIndex $ (:[]) $ do
			d <- newName "d"
			clause [conP dartN [wildP, varP d]] (normalB $ varE d) []
		]

	declare $ instanceD (cxt []) ([t|KnottedWithAccel|] `appT` conT knotTN `appT` conT crosN `appT` conT dartN)
		[ funD 'forMIncidentDarts $ (:[]) $ do
			k <- newName "k"
			c <- newName "c"
			f <- newName "f"
			clause [conP crosN [varP k, varP c], varP f] (normalB [|
				let b = $(varE c) `shiftL` 2
				in $(varE f) ($(conE dartN) $(varE k) b) >> $(varE f) ($(conE dartN) $(varE k) $! b + 1)
					>> $(varE f) ($(conE dartN) $(varE k) $! b + 2) >> $(varE f) ($(conE dartN) $(varE k) $! b + 3)
				|]) []

		, funD 'foldMIncidentDarts $ (:[]) $ do
			k <- newName "k"
			c <- newName "c"
			f <- newName "f"
			s <- newName "s"
			clause [conP crosN [varP k, varP c], varP f, varP s] (normalB [|
				let b = $(varE c) `shiftL` 2
				in $(varE f) ($(conE dartN) $(varE k) b) $(varE s)
					>>= $(varE f) ($(conE dartN) $(varE k) $! b + 1)
						>>= $(varE f) ($(conE dartN) $(varE k) $! b + 2)
							>>= $(varE f) ($(conE dartN) $(varE k) $! b + 3)
				|]) []

		, funD 'foldMIncidentDartsFrom $ (:[]) $ do
			dart <- newName "dart"
			k <- newName "k"
			i <- newName "i"
			dir <- newName "dir"
			f <- newName "f"
			s <- newName "s"
			clause [asP dart $ conP dartN [varP k, varP i], bangP $ varP dir, varP f, bangP $ varP s] (normalB $
				maybe id ($ (varE dart, (varE k, varE i))) (modifyFoldMIncidentDartsFrom inst) [|
					let	d = directionSign $(varE dir)
						r = $(varE i) .&. complement 3
						l1 = ($(varE i) + d) .&. 3 ; l2 = (l1 + d) .&. 3 ; l3 = (l2 + d) .&. 3
					in $(varE f) $(varE dart) $(varE s)
						>>= $(varE f) ($(conE dartN) $(varE k) $! r + l1)
							>>= $(varE f) ($(conE dartN) $(varE k) $! r + l2)
								>>= $(varE f) ($(conE dartN) $(varE k) $! r + l3)
					|]
				) []
		]

	declare $ do
		ct <- newName "ct"
		sigD (mkName "changeNumberOfFreeLoops") $ forallT [PlainTV ct] (cxt [])
			[t| $(conT knotTN) $(varT ct) -> Int -> $(conT knotTN) $(varT ct) |]

	declare $ funD (mkName "changeNumberOfFreeLoops") $ (:[]) $ do
		loops <- newName "loops"
		knot <- newName "knot"
		clause [varP knot, varP loops] (normalB $ recUpdE (varE knot) [(,) loopsCount `fmap` varE loops]) []

	maybeM (implodeExplodeSettings inst) $ \ ies -> do
		declare $ do
			ct <- newName "ct"
			sigD (mkName "implode") $ forallT [PlainTV ct] (cxt [classP ''CrossingType [varT ct]])
				[t| (Int, [([(Int, Int)], CrossingState $(varT ct))]) -> $(conT knotTN) $(varT ct) |]

		declare $ funD (mkName "implode") $ (:[]) $ do
			arg <- newName "arg"
			loops <- newName "loops"
			list <- newName "list"
			clause [asP arg $ tupP [bangP $ varP loops, bangP $ varP list]] (normalB [|
				runST $ do
					when ($(varE loops) < (0 :: Int)) $ fail $
						printf "implode: number of free loops is negative (%i)" $(varE loops)

					let n = length $(varE list)
					cr <- newArray_ (0, 4 * n - 1) :: ST s (STUArray s Int Int)
					st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)

					forM_ (zip $(varE list) [0 ..]) $ \ ((!ns, !state), !i) -> do
						unsafeWrite st i state
						when (length ns /= 4) $ fail $
							printf "implode: there must be 4 neighbours for every crossing, but found %i at %i" (length ns) (i + 1)
						forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
							when (c < 1 || c > n || p < 0 || p > 3) $ fail "implode: out of bound"
							let a = 4 * i + j
							let b = 4 * (c - 1) + p
							when (a == b) $ fail $
								printf "implode: dart (%i, %i) connected to itself in %s" (i + 1) j (show $(varE arg))
							unsafeWrite cr a b
							when (b < a) $ do
								x <- unsafeRead cr b
								when (x /= a) $ fail $
									printf "implode: unconsistent data in %s" (show $(varE arg))

					cr' <- unsafeFreeze cr
					st' <- unsafeFreeze st

					$(doE $ implodeExtra ies ++
						[ noBindS $ [| return $! $(recConE knotCN $
							[ (,) crossCount `fmap` [|n|]
							, (,) crossArray `fmap` [|cr'|]
							, (,) stateArray `fmap` [|st'|]
							, (,) loopsCount `fmap` varE loops
							] ++ implodeInitializers ies)
						|] ])
				|]) []

		declare $ do
			ct <- newName "ct"
			sigD (mkName "explode") $ forallT [PlainTV ct] (cxt [classP ''CrossingType [varT ct]])
				[t| $(conT knotTN) $(varT ct) -> (Int, [([(Int, Int)], CrossingState $(varT ct))]) |]

		declare $ funD (mkName "explode") $ (:[]) $ do
			l <- newName "l"
			clause [varP l] (normalB [|
				( numberOfFreeLoops $(varE l)
				, map (\ c -> (map (toPair . opposite) $ incidentDarts c, crossingState c)) $ allCrossings $(varE l)
				)
				|]) []
