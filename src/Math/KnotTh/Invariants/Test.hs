module Math.KnotTh.Invariants.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, test)
import Math.KnotTh.Link.Table
import Math.KnotTh.Link.GaussCode (fromDTCode)
import Math.KnotTh.Tangle.Table
import Math.KnotTh.Tangle.CascadeCode
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.KauffmanFPolynomial


test :: Test
test = testGroup "Invariants"
    [ testGroup "Linking numbers of link" $
        map (\ (name, l, target) -> testCase name $ linkingNumbersSet l @?= target)
            [ ("whitehead link" , whiteheadLink     , [0]      )
            , ("hopf link"      , hopfLink          , [2]      )
            , ("borromean rings", borromeanRingsLink, [0, 0, 0])
            ]

    , testGroup "Jones polynomial of link" $
        map (\ (name, l, target) -> testCase name $ show (normalizedJonesPolynomialOfLink l) @?= target)
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
            , ("stevedore knot"         , stevedoreKnot        , "t^-4-t^-3+t^-2-2t^-1+2-t+t^2"                  )
            , ("6_2 knot"               , knot 6 2             , "t^-5-2t^-4+2t^-3-2t^-2+2t^-1-1+t"              )
            , ("6_3 kont"               , knot 6 3             , "-t^-3+2t^-2-2t^-1+3-2t+2t^2-t^3"               )
            , ("borromean rings"        , borromeanRingsLink   , "-t^-3+3t^-2-2t^-1+4-2t+3t^2-t^3"               )
            , ("Conway knot"            , conwayKnot           , "-t^-4+2t^-3-2t^-2+2t^-1+t^2-2t^3+2t^4-2t^5+t^6")
            , ("Kinoshita-Terasaka knot", kinoshitaTerasakaKnot, "-t^-4+2t^-3-2t^-2+2t^-1+t^2-2t^3+2t^4-2t^5+t^6")

            , ( "12n_0801"
              , fromDTCode [6, 10, -18, 22, 2, -16, -24, -20, -4, -12, 8, -14]
              , "t^-11-2t^-10+3t^-9-4t^-8+3t^-7-3t^-6+2t^-5-t^-4+t^-3+t^-2"
              )

            , ( "12n_0819"
              , fromDTCode [6, 10, 20, -14, 2, 18, 24, -8, 22, 4, 12, 16]
              , "-t^-8+3t^-7-7t^-6+12t^-5-16t^-4+18t^-3-17t^-2+15t^-1-10+6t-2t^2"
              )

            , ( "12n_0820"
              , fromDTCode [6, -10, -20, 16, -2, -18, 22, 24, 8, -4, 12, 14]
              , "2t^3-4t^4+8t^5-11t^6+14t^7-15t^8+14t^9-11t^10+7t^11-4t^12+t^13"
              )
            ]

    , testGroup "Jones polynomial of tangle" $
        map (\ (name, t, target) -> testCase name $ show (jonesPolynomial t) @?= target)
            [ ("zero"          , zeroTangle                 , "[(1)[3,2,1,0]]"                       )
            , ("infinity"      , infinityTangle             , "[(1)[1,0,3,2]]"                       )
            , ("over crossing" , lonerOverCrossingTangle    , "[(t^1/4)[1,0,3,2],(t^-1/4)[3,2,1,0]]" )
            , ("under crossing", lonerUnderCrossingTangle   , "[(t^-1/4)[1,0,3,2],(t^1/4)[3,2,1,0]]" )
            , ("group 2"       , groupTangle 2              , "[(1-t)[1,0,3,2],(t^-1/2)[3,2,1,0]]"   )
            , ("group -2"      , groupTangle (-2)           , "[(-t^-1+1)[1,0,3,2],(t^1/2)[3,2,1,0]]")
            , ("II reducable"  , decodeCascadeCode [(XU, 0)], "[(1)[3,2,1,0]]"                       )
            ]

    , testGroup "Kauffman X polynomial of link" $
        map (\ (name, l, target) -> testCase name $ show (kauffmanXPolynomial l) @?= target)
            [ ("unknot"           , unknot              , "-A^-2-A^2"         )
            , ("unknot left '8'"  , singleCrossingUnknot, "-A^-2-A^2"         )
            , ("left trefoil knot", leftTrefoilKnot     , "-A^2-A^6-A^10+A^18")
            , ("figure eight knot", figureEightKnot     , "-A^-10-A^10"       )
            , ("hopf link"        , hopfLink            , "A^-6+A^-2+A^2+A^6" )
            ]

    , testGroup "Kauffman F polynomial of link" $
        map (\ (name, l, target) -> testCase name $ show ({-normalizedKauffmanFPolynomialOfLink-} bruteForceKauffmanF l) @?= target)
            [ ("unknot"             , unknot                              , "1"                                                                                       )
            , ("unknot left '8'"    , singleCrossingUnknot                , "1"                                                                                       )
            , ("unknot right '8'"   , invertCrossings singleCrossingUnknot, "1"                                                                                       )
            , ("left trefoil knot"  , rightTrefoilKnot                    , "a^-5z-a^-4+a^-4z^2+a^-3z-2a^-2+a^-2z^2"                                                  )
            , ("figure eight knot"  , figureEightKnot                     , "-a^-2+a^-2z^2-a^-1z-1+a^-1z^3+2z^2-az-a^2+az^3+a^2z^2"                                   )
            , ("solomon's seal knot", rightCinquefoilKnot                 , "a^-9z+a^-8z^2-a^-7z+2a^-6+a^-7z^3-3a^-6z^2-2a^-5z+3a^-4+a^-6z^4+a^-5z^3-4a^-4z^2+a^-4z^4")
            , ("hopf link"          , hopfLink                            , "-a^-1z^-1+a^-1z+1-az^-1+az"                                                              )
            ]
    ]
