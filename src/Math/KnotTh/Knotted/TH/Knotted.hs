{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Knotted.TH.Knotted
	( KnottedInstance(..)
	, defaultKnottedInstance
	, produceKnottedInstance
	) where

import Language.Haskell.TH
import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STArray, STUArray, runSTArray, newArray_)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Show


data KnottedInstance = KnottedInstance
	{ modifyNumberOfEdges :: ExpQ -> ExpQ -> ExpQ
	, toListExtra         :: ([StmtQ], [Q (Name, Exp)])
	}


defaultKnottedInstance :: KnottedInstance
defaultKnottedInstance = KnottedInstance
	{ modifyNumberOfEdges = const id
	, toListExtra         = ([], [])
	}


produceKnottedInstance :: DecsQ -> KnottedInstance -> DecsQ
produceKnottedInstance knotPattern inst = do
	[DataD [] knotTN [PlainTV crossType] [RecC knotCN knotFields] []] <- knotPattern

	let dartN = mkName "Dart"
	let crosN = mkName "Crossing"

	loopsCount <- newName "loopsCount"
	crossCount <- newName "crossCount"
	stateArray <- newName "stateArray"
	crossArray <- newName "crossArray"

	let dartKnotN = mkName $ "dart" ++ nameBase knotTN
	let crosKnotN = mkName $ "crossing" ++ nameBase knotTN

	sequence
		[ dataD (cxt []) knotTN [PlainTV crossType] [recC knotCN $ map return (
			[ (loopsCount, Unpacked, ConT ''Int)
			, (crossCount, Unpacked, ConT ''Int)
			, (crossArray, Unpacked, ConT ''UArray `AppT` ConT ''Int `AppT` ConT ''Int)
			, (stateArray, Unpacked, ConT ''Array `AppT` ConT ''Int `AppT` (ConT ''CrossingState `AppT` VarT crossType))
			] ++ knotFields)] []
		
		, do
			ct <- newName "ct"
			dataD (cxt []) dartN [PlainTV ct] [normalC dartN 
				[ (,) IsStrict `fmap` (conT knotTN `appT` varT ct)
				, (,) Unpacked `fmap` conT ''Int
				]] []

		, do
			ct <- newName "ct"
			sigD dartKnotN $ forallT [PlainTV ct] (cxt [])
				[t| $(conT dartN) $(varT ct) -> $(conT knotTN) $(varT ct) |]

		, return $ PragmaD $ InlineP dartKnotN $ InlineSpec True False Nothing

		, funD dartKnotN $ (:[]) $ do
			k <- newName "k"
			clause [conP dartN [varP k, wildP]] (normalB $ varE k) []

		, instanceD (cxt []) (appT [t|Eq|] $ conT dartN `appT` varT (mkName "ct"))
			[ funD '(==) $ (:[]) $ do
				a <- newName "a"
				b <- newName "b"
				clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]] (normalB [|
					$(varE a) == $(varE b)
					|]) []
			]

		, instanceD (cxt []) (appT [t|Ord|] $ conT dartN `appT` varT (mkName "ct"))
			[ funD 'compare $ (:[]) $ do
				a <- newName "a"
				b <- newName "b"
				clause [conP dartN [wildP, varP a], conP dartN [wildP, varP b]] (normalB [|
					$(varE a) `compare` $(varE b)
					|]) []
			]


		, do
			ct <- newName "ct"
			dataD (cxt []) crosN [PlainTV ct] [normalC crosN
				[ (,) IsStrict `fmap` (conT knotTN `appT` varT ct)
				, (,) Unpacked `fmap` conT ''Int
				]] []

		, do
			ct <- newName "ct"
			sigD crosKnotN $ forallT [PlainTV ct] (cxt [])
				[t| $(conT crosN) $(varT ct) -> $(conT knotTN) $(varT ct) |]

		, return $ PragmaD $ InlineP crosKnotN $ InlineSpec True False Nothing

		, funD crosKnotN $ (:[]) $ do
			k <- newName "k"
			clause [conP crosN [varP k, wildP]] (normalB $ varE k) []

		, instanceD (cxt []) (appT [t|Eq|] $ conT crosN `appT` varT (mkName "ct"))
			[ funD '(==) $ (:[]) $ do
				a <- newName "a"
				b <- newName "b"
				clause [conP crosN [wildP, varP a], conP crosN [wildP, varP b]] (normalB [|
					$(varE a) == $(varE b)
					|]) []
			]

		, instanceD (cxt []) (appT [t|Ord|] $ conT crosN `appT` varT (mkName "ct"))
			[ funD 'compare $ (:[]) $ do
				a <- newName "a"
				b <- newName "b"
				clause [conP crosN [wildP, varP a], conP crosN [wildP, varP b]] (normalB [|
					$(varE a) `compare` $(varE b)
					|]) []
			]


		, instanceD (cxt []) ([t|Knotted|] `appT` conT knotTN `appT` conT crosN `appT` conT dartN)
			[ funD 'numberOfFreeLoops $ (:[]) $ clause [] (normalB $ varE loopsCount) []

			, funD 'numberOfCrossings $ (:[]) $ clause [] (normalB $ varE crossCount) []

			, funD 'numberOfEdges $ (:[]) $ do
				k <- newName "k"
				clause [varP k] (normalB $ modifyNumberOfEdges inst (varE k) [| numberOfCrossings $(varE k) * (2 :: Int) |]) []

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
						then error "nthCrossing: out of bound"
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

			, funD 'dartOwner $ (:[]) $ do
				k <- newName "k"
				clause [conP dartN [varP k, wildP]] (normalB $ varE k) []

			, funD 'dartPlace $ (:[]) $ do
				d <- newName "d"
				clause [conP dartN [wildP, varP d]] (normalB [|
					$(varE d) .&. (3 :: Int)
					|]) []

			, funD 'incidentCrossing $ (:[]) $ do
				k <- newName "k"
				d <- newName "d"
				clause [conP dartN [varP k, varP d]] (normalB [|
					$(conE crosN) $(varE k) $! $(varE d) `shiftR` (2 :: Int)
					|]) []

			, funD 'opposite $ (:[]) $ do
				k <- newName "k"
				d <- newName "d"
				clause [conP dartN [varP k, varP d]] (normalB [|
					$(conE dartN) $(varE k) $! $(varE crossArray) $(varE k) `unsafeAt` $(varE d)
					|]) []

			, funD 'nextCCW $ (:[]) $ do
				k <- newName "k"
				d <- newName "d"
				clause [conP dartN [varP k, varP d]] (normalB [|
					$(conE dartN) $(varE k) $! ($(varE d) .&. complement 3 :: Int) + (($(varE d) + 1) .&. 3 :: Int)
					|]) []

			, funD 'nextCW $ (:[]) $ do
				k <- newName "k"
				d <- newName "d"
				clause [conP dartN [varP k, varP d]] (normalB [|
					$(conE dartN) $(varE k) $! ($(varE d) .&. complement 3 :: Int) + (($(varE d) - 1) .&. 3 :: Int)
					|]) []

			, funD 'dartArrIndex $ (:[]) $ do
				d <- newName "d"
				clause [conP dartN [wildP, varP d]] (normalB $ varE d) []
			]

		, instanceD (cxt []) ([t|KnottedWithAccel|] `appT` conT knotTN `appT` conT crosN `appT` conT dartN)
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
					in $(varE f) ($(conE dartN) $(varE k) b) $(varE s) >>= $(varE f) ($(conE dartN) $(varE k) $! b + 1)
						>>= $(varE f) ($(conE dartN) $(varE k) $! b + 2) >>= $(varE f) ($(conE dartN) $(varE k) $! b + 3)
					|]) []

			, funD 'foldMIncidentDartsFrom $ (:[]) $ do
				dart <- newName "dart"
				k <- newName "k"
				i <- newName "i"
				dir <- newName "dir"
				f <- newName "f"
				s <- newName "s"
				clause [asP dart $ conP dartN [varP k, varP i], bangP $ varP dir, varP f, bangP $ varP s] (normalB [|
					let	d = directionSign $(varE dir)
						r = $(varE i) .&. complement 3
					in $(varE f) $(varE dart) $(varE s) >>= $(varE f) ($(conE dartN) $(varE k) $! r + (($(varE i) + d) .&. 3))
						>>= $(varE f) ($(conE dartN) $(varE k) $! r + (($(varE i) + 2 * d) .&. 3))
							>>= $(varE f) ($(conE dartN) $(varE k) $! r + (($(varE i) + 3 * d) .&. 3))
					|]) []
			]

		, do
			ct <- newName "ct"
			sigD (mkName "fromList") $ forallT [PlainTV ct] (cxt [])
				[t| (Int, [([(Int, Int)], CrossingState $(varT ct))]) -> $(conT knotTN) $(varT ct) |]

		, funD (mkName "fromList") $ (:[]) $ do
			loops <- newName "loops"
			list <- newName "list"
			clause [tupP [bangP $ varP loops, bangP $ varP list]] (normalB [|
				runST $ do
					when ($(varE loops) < (0 :: Int)) (fail "fromListST: number of free loops is negative")

					let n = length $(varE list)
					cr <- newArray_ (0, 4 * n - 1) :: ST s (STUArray s Int Int)
					st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)

					forM_ (zip $(varE list) [0 ..]) $ \ ((!ns, !state), !i) -> do
						unsafeWrite st i state
						when (length ns /= 4) (fail "fromList: there must be 4 neighbours for every crossing")
						forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
							when (c < 1 || c > n || p < 0 || p > 3) (fail "fromList: out of bound")
							let a = 4 * i + j
							let b = 4 * (c - 1) + p
							when (a == b) (fail "fromList: dart connected to itself")
							unsafeWrite cr a b
							when (b < a) $ do
								x <- unsafeRead cr b
								when (x /= a) (fail "fromList: unconsistent data")

					cr' <- unsafeFreeze cr
					st' <- unsafeFreeze st

					$(doE $ fst (toListExtra inst) ++
						[ noBindS $ [| return $! $(recConE knotCN $
							[ (,) crossCount `fmap` [|n|]
							, (,) crossArray `fmap` [|cr'|]
							, (,) stateArray `fmap` [|st'|]
							, (,) loopsCount `fmap` varE loops
							] ++ snd (toListExtra inst))
						|] ])
				|]) []

		, produceShowDart dartN

		, produceShowCrossing crosN

		, produceShowKnot knotTN
		]
