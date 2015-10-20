module Math.Topology.KnotTh.Invariants.Test
    ( test
    , testHard
    ) where

import Control.Monad (forM_)
#ifdef TESTING
import qualified Data.Map as M
#endif
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Text.Printf
#ifdef TESTING
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LMP
#endif
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Invariants
#ifdef TESTING
import Math.Topology.KnotTh.Invariants.Util.Poly
#endif


#ifdef TESTING
testKauffmanFtoXRelation :: (String, LinkDiagram) -> Assertion
testKauffmanFtoXRelation =
    let z = monomial2 1 "z" 1
        z' = monomial 1 "a" 1 + monomial 1 "a" (-1)

        toKauffmanX (LMP.LP monomials) =
            sum $ flip map monomials $ \ (LMP.LM m, f) ->
                (fromIntegral f *) $ product $ flip map (M.toList m) $ \ (var, p) ->
                    let x = case var of
                            "a" | p >= 0    -> monomial (-1) "a" 3
                                | otherwise -> monomial (-1) "a" (-3)
                            "z"             -> z'
                            _               -> undefined
                    in x ^ abs (B.numeratorQ p)
    in \ (name, link) ->
        let kf = kauffmanFPolynomial link
            kx = kauffmanXPolynomial link
            n = numberOfVertices link -- To get rid of negative z exponents
        in assertEqual (printf "on %s: %s vs %s" name (show kf) (show kx))
                       (kx * (z' ^ n))
                       (toKauffmanX $ kf * (z ^ n))
#endif


testReidemeisterMoves :: (Eq a, Show a) => (TangleDiagram -> a) -> Test
testReidemeisterMoves invariant =
    testGroup "Reidemeister moves"
        [ testCase "Reidemeister I" $
            forM_ reidemeisterIExamples $ \ (before, after) ->
                invariant (toTangle after) @?= invariant (toTangle before)

        , testCase "Reidemeister II" $
            forM_ reidemeisterIIExamples $ \ (before, after) ->
                invariant (toTangle after) @?= invariant (toTangle before)

        , testCase "Reidemeister III" $
            forM_ reidemeisterIIIExamples $ \ (before, after) ->
                invariant (toTangle after) @?= invariant (toTangle before)
        ]


test :: Test
test = testGroup "Invariants"
    [ testGroup "Linking numbers" $ do
        (header, l, target) <-
            [ ("whitehead link" , whiteheadLink     , [0]      )
            , ("hopf link"      , hopfLink          , [2]      )
            , ("borromean rings", borromeanRingsLink, [0, 0, 0])
            ]

        return $ testCase header $
            linkingNumbersInvariant l @?= target

    , testGroup "Kauffman X (Jones) polynomial"
        [ testReidemeisterMoves kauffmanXPolynomial

        , testGroup "Jones on a few special knots and links" $ do
            (header, l, target) <-
                [ ("unknot"                 , unknot               , "1"                                             )
                , ("unknot '8'"             , singleCrossingUnknot , "1"                                             )
                , ("left trefoil knot"      , leftTrefoilKnot      , "-t^-4+t^-3+t^-1"                               )
                , ("right trefoil knot"     , rightTrefoilKnot     , "t+t^3-t^4"                                     )
                , ("figure eight knot"      , figureEightKnot      , "t^-2-t^-1+1-t+t^2"                             )
                , ("hopf link"              , hopfLink             , "-t^-1-t"                                       )
                , ("solomon's seal knot"    , rightCinquefoilKnot  , "t^2+t^4-t^5+t^6-t^7"                           )
                , ("granny knot"            , grannyKnot           , "t^2+2t^4-2t^5+t^6-2t^7+t^8"                    )
                , ("square knot"            , squareKnot           , "-t^-3+t^-2-t^-1+3-t+t^2-t^3"                   )
                , ("whitehead link"         , whiteheadLink        , "t^-7/2-2t^-5/2+t^-3/2-2t^-1/2+t^1/2-t^3/2"     )
                , ("three-twist knot"       , threeTwistKnot       , "-t^-6+t^-5-t^-4+2t^-3-t^-2+t^-1"               )
                , ("borromean rings"        , borromeanRingsLink   , "-t^-3+3t^-2-2t^-1+4-2t+3t^2-t^3"               )
                , ("Conway knot"            , conwayKnot           , "-t^-4+2t^-3-2t^-2+2t^-1+t^2-2t^3+2t^4-2t^5+t^6")
                , ("Kinoshita-Terasaka knot", kinoshitaTerasakaKnot, "-t^-4+2t^-3-2t^-2+2t^-1+t^2-2t^3+2t^4-2t^5+t^6")

                , ( "12n_0801"
                  , flipCrossings $ fromDTCode [[6, 10, -18, 22, 2, -16, -24, -20, -4, -12, 8, -14]]
                  , "t^-11-2t^-10+3t^-9-4t^-8+3t^-7-3t^-6+2t^-5-t^-4+t^-3+t^-2"
                  )

                , ( "12n_0819"
                  , flipCrossings $ fromDTCode [[6, 10, 20, -14, 2, 18, 24, -8, 22, 4, 12, 16]]
                  , "-t^-8+3t^-7-7t^-6+12t^-5-16t^-4+18t^-3-17t^-2+15t^-1-10+6t-2t^2"
                  )

                , ( "12n_0820"
                  , flipCrossings $ fromDTCode [[6, -10, -20, 16, -2, -18, 22, 24, 8, -4, 12, 14]]
                  , "2t^3-4t^4+8t^5-11t^6+14t^7-15t^8+14t^9-11t^10+7t^11-4t^12+t^13"
                  )
                ]

            return $ testCase header $
                show (normalizedJonesPolynomialOfLink l) @?= target

        , testGroup "Jones on knots and links from table" $ do
            ((cross, comps), polys) <-
                [ ((2, 2),
                    [ "-t^-1-t"
                    ])

                , ((3, 1),
                    [ "t+t^3-t^4"
                    ])

                , ((4, 1),
                    [ "t^-2-t^-1+1-t+t^2"
                    ])

                , ((5, 1),
                    [ "t^2+t^4-t^5+t^6-t^7"
                    , "t-t^2+2t^3-t^4+t^5-t^6"
                    ])

                , ((6, 1),
                    [ "t^-2-t^-1+2-2t+t^2-t^3+t^4"
                    , "t^-1-1+2t-2t^2+2t^3-2t^4+t^5"
                    , "-t^-3+2t^-2-2t^-1+3-2t+2t^2-t^3"
                    ])

                , ((7, 1),
                    [ "t^3+t^5-t^6+t^7-t^8+t^9-t^10"
                    , "t-t^2+2t^3-2t^4+2t^5-t^6+t^7-t^8"
                    , "t^2-t^3+2t^4-2t^5+3t^6-2t^7+t^8-t^9"
                    , "t-2t^2+3t^3-2t^4+3t^5-2t^6+t^7-t^8"
                    , "t^2-t^3+3t^4-3t^5+3t^6-3t^7+2t^8-t^9"
                    , "t^-1-2+3t-3t^2+4t^3-3t^4+2t^5-t^6"
                    , "t^-4-2t^-3+3t^-2-4t^-1+4-3t+3t^2-t^3"
                    ])

                , ((8, 1),
                    [ "t^-2-t^-1+2-2t+2t^2-2t^3+t^4-t^5+t^6"
                    , "1-t+2t^2-2t^3+3t^4-3t^5+2t^6-2t^7+t^8"
                    , "t^-4-t^-3+2t^-2-3t^-1+3-3t+2t^2-t^3+t^4"
                    , "t^-5-2t^-4+3t^-3-3t^-2+3t^-1-3+2t-t^2+t^3"
                    , "1-t+3t^2-3t^3+3t^4-4t^5+3t^6-2t^7+t^8"
                    , "t^-1-1+3t-4t^2+4t^3-4t^4+3t^5-2t^6+t^7"
                    , "-t^-6+2t^-5-3t^-4+4t^-3-4t^-2+4t^-1-2+2t-t^2"
                    , "-t^-5+2t^-4-3t^-3+4t^-2-4t^-1+5-3t+2t^2-t^3"
                    , "t^-4-2t^-3+3t^-2-4t^-1+5-4t+3t^2-2t^3+t^4"
                    , "-t^-6+2t^-5-4t^-4+5t^-3-4t^-2+5t^-1-3+2t-t^2"
                    , "t^-1-2+4t-4t^2+5t^3-5t^4+3t^5-2t^6+t^7"
                    , "t^-4-2t^-3+4t^-2-5t^-1+5-5t+4t^2-2t^3+t^4"
                    , "-t^-5+2t^-4-3t^-3+5t^-2-5t^-1+5-4t+3t^2-t^3"
                    , "t^-1-2+4t-5t^2+6t^3-5t^4+4t^5-3t^6+t^7"
                    , "t^2-2t^3+5t^4-5t^5+6t^6-6t^7+4t^8-3t^9+t^10"
                    , "-t^-6+3t^-5-5t^-4+6t^-3-6t^-2+6t^-1-4+3t-t^2"
                    , "t^-4-3t^-3+5t^-2-6t^-1+7-6t+5t^2-3t^3+t^4"
                    , "t^-4-4t^-3+6t^-2-7t^-1+9-7t+6t^2-4t^3+t^4"
                    , "t^3+t^5-t^8"
                    , "-t^-5+t^-4-t^-3+2t^-2-t^-1+2-t"
                    ])
                ]

            (link, poly, number) <- zip3 (linkTable cross comps) polys [1 :: Int ..]
            let header | comps == 1  = printf "knot %i_%i" cross number
                       | otherwise   = printf "link %i^%i_%i" cross comps number
            return $ testCase header $
                show (normalizedJonesPolynomialOfLink link) @?= poly

        , testGroup "Kauffman X values on a few tangles" $ do
            (header, t, target) <-
                [ ("empty"         , planarEmpty                   , "(1)[]"                            )
                , ("identity"      , planarPropagator 1            , "(1)[1,0]"                         )
                , ("0"             , toTangle zeroTangle           , "(1)[3,2,1,0]"                     )
                , ("∞"             , toTangle infinityTangle       , "(1)[1,0,3,2]"                     )
                , ("over crossing" , toTangle lonerOverCrossing    , "(a^-1)[1,0,3,2]+(a)[3,2,1,0]"     )
                , ("under crossing", toTangle lonerUnderCrossing   , "(a)[1,0,3,2]+(a^-1)[3,2,1,0]"     )
                , ("group 2"       , toTangle $ rationalTangle [2] , "(1-a^4)[1,0,3,2]+(a^-2)[3,2,1,0]" )
                , ("group -2"      , toTangle $ rationalTangle [-2], "(-a^-4+1)[1,0,3,2]+(a^2)[3,2,1,0]")
                , ("II reducable"  , decodeCascadeCode [(XU, 0)]   , "(1)[3,2,1,0]"                     )
                ]

            return $ testCase header $
                show (kauffmanXPolynomial t) @?= target

        , testGroup "Kauffman X polynomial" $ do
            (header, l, target) <-
                [ ("unknot"           , unknot              , "-a^-2-a^2"         )
                , ("unknot left '8'"  , singleCrossingUnknot, "-a^-2-a^2"         )
                , ("left trefoil knot", leftTrefoilKnot     , "-a^2-a^6-a^10+a^18")
                , ("figure eight knot", figureEightKnot     , "-a^-10-a^10"       )
                , ("hopf link"        , hopfLink            , "a^-6+a^-2+a^2+a^6" )
                ]

            return $ testCase header $
                show (kauffmanXPolynomial l) @?= target

        , testCase "Collision between Conway and Kinoshita-Terasaka knots" $
            kauffmanXPolynomial conwayKnot @?= kauffmanXPolynomial kinoshitaTerasakaKnot
        ]

    , testGroup "Kauffman F polynomial"
        [ testReidemeisterMoves kauffmanFPolynomial

        , testGroup "Exact values on links" $ do
            (header, l, target) <-
                [ ("unknot"             , unknot                            , "1"                                                                                       )
                , ("unknot left '8'"    , singleCrossingUnknot              , "1"                                                                                       )
                , ("unknot right '8'"   , flipCrossings singleCrossingUnknot, "1"                                                                                       )
                , ("right trefoil knot" , rightTrefoilKnot                  , "a^-5z-a^-4+a^-4z^2+a^-3z-2a^-2+a^-2z^2"                                                  )
                , ("figure eight knot"  , figureEightKnot                   , "-a^-2+a^-2z^2-a^-1z-1+a^-1z^3+2z^2-az-a^2+az^3+a^2z^2"                                   )
                , ("solomon's seal knot", rightCinquefoilKnot               , "a^-9z+a^-8z^2-a^-7z+2a^-6+a^-7z^3-3a^-6z^2-2a^-5z+3a^-4+a^-6z^4+a^-5z^3-4a^-4z^2+a^-4z^4")
                , ("three twist knot"   , threeTwistKnot                    , "-a^2+a^2z^2+a^4+a^3z^3-a^4z^2-2a^5z+a^6+a^4z^4+2a^5z^3-2a^6z^2-2a^7z+a^6z^4+a^7z^3"      )
                , ("hopf link"          , hopfLink                          , "-a^-1z^-1+a^-1z+1-az^-1+az"                                                              )
                ]

            return $ testCase header $
                show (normalizedKauffmanFPolynomialOfLink l) @?= target

        , testGroup "Exact values on tangles" $ do
            (header, t, target) <-
                [ ("empty"         , planarEmpty                , "(1)[]"                                  )
                , ("identity"      , planarPropagator 1         , "(1)[1,0]"                               )
                , ("0"             , toTangle zeroTangle        , "(1)[3,2,1,0]"                           )
                , ("∞"             , toTangle infinityTangle    , "(1)[1,0,3,2]"                           )
                , ("over crossing" , toTangle lonerOverCrossing , "(1)[2,3,0,1]"                           )
                , ("under crossing", toTangle lonerUnderCrossing, "(z)[1,0,3,2]+(-1)[2,3,0,1]+(z)[3,2,1,0]")
                ]

            return $ testCase header $
                show (kauffmanFPolynomial t) @?= target

#ifdef TESTING
        , testCase "Relation to Kauffman X polynomial" $
            mapM_ testKauffmanFtoXRelation
                [ ("right trefoil knot"     , rightTrefoilKnot     )
                , ("left trefoil knot"      , leftTrefoilKnot      )
                , ("figure eight knot"      , figureEightKnot      )
                , ("hopf link"              , hopfLink             )
                , ("solomon's seal knot"    , rightCinquefoilKnot  )
                , ("granny knot"            , grannyKnot           )
                , ("square knot"            , squareKnot           )
                , ("whitehead link"         , whiteheadLink        )
                , ("three-twist knot"       , threeTwistKnot       )
                , ("stevedore knot"         , stevedoreKnot        )
                , ("6_2 knot"               , knotT 6 2            )
                , ("6_3 knot"               , knotT 6 3            )
                , ("borromean rings"        , borromeanRingsLink   )
                , ("Conway knot"            , conwayKnot           )
                , ("Kinoshita-Terasaka knot", kinoshitaTerasakaKnot)
                ]
#endif

        , testCase "Collision between Conway and Kinoshita-Terasaka knots" $
            kauffmanFPolynomial conwayKnot @?= kauffmanFPolynomial kinoshitaTerasakaKnot
        ]

    , testGroup "Khovanov homology"
        [ testCase "∂∘∂" $ do
            mapM_ (\ tangle ->
                    let kh = khovanovComplex tangle
                    in assertBool (printf "∂∘∂ failed at: %s" (show tangle)) $ testComplexBorders kh
                )
                [ toTangle lonerOverCrossing
                , toTangle lonerUnderCrossing
                , toTangle $ rationalTangle [2]
                , toTangle $ rationalTangle [3]
                , toTangle $ rationalTangle [-2]
                , toTangle $ rationalTangle [1, 2]
                , toTangle $ rationalTangle [2, 1, 1]
                , toTangle $ rationalTangle [-4]
                , toTangle $ rationalTangle [2, 3, -1]
                , toTangle leftTrefoilKnot
                ]

        , testCase "unknot" $ do
            khovanovHomologyBetti (toTangle unknot) @?= [(0, 1)]

        , testCase "unknot 8" $ do
            khovanovHomologyBetti (toTangle singleCrossingUnknot) @?= [(0, 2)]

        , testCase "left trefoil knot" $
            khovanovHomologyBetti (toTangle leftTrefoilKnot) @?= [(-3, 1), (-2, 1), (0, 2)]

        , testCase "right trefoil knot" $
            khovanovHomologyBetti (toTangle rightTrefoilKnot) @?= [(0, 2), (2, 1), (3, 1)]

        , testCase "figure eight knot" $
            khovanovHomologyBetti (toTangle figureEightKnot) @?= [(-2, 1), (-1, 1), (0, 2), (1, 1), (2, 1)]

        , testCase "left cinquefoil knot" $
            khovanovHomologyBetti (toTangle leftCinquefoilKnot) @?= [(-5, 1), (-4, 1), (-3, 1), (-2, 1), (0, 2)]

        , testCase "three twist knot" $
            khovanovHomologyBetti (toTangle threeTwistKnot) @?= [(-5, 1), (-4, 1), (-3, 1), (-2, 2), (-1, 1), (0, 2)]

        , testCase "hopf link" $
            khovanovHomologyBetti (toTangle hopfLink) @?= [(0, 2), (2, 2)]

        , testCase "borromean rings" $
            khovanovHomologyBetti (toTangle borromeanRingsLink) @?= [(0, 1), (1, 3), (2, 2), (3, 8), (4, 2), (5, 3), (6, 1)]

        , testReidemeisterMoves khovanovHomologyBetti
        ]
    ]


testHard :: Test
testHard =
    testGroup "Invariants"
        [
#ifdef TESTING
          testCase "Relation to Kauffman X polynomial on entire links table" $
            mapM_ testKauffmanFtoXRelation $ do
                n <- [0 .. 10]
                l <- [0 .. n `div` 2]
                (link, i) <- linkTable n l `zip` [1 :: Int ..]
                return $! (printf "link %i^%i_%i" n l i, link)
#endif
        ]
