{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Suli.Nyelv.Verslab

main :: IO ()
main = defaultMain $ testGroup "Tesztek"
    [ testGroup "Versláb"
        [ testGroup "Szótagolás"
            [ testCase "Alma" $
                idoSzotagol "Alma" @?= ["Alm", "a"]
            , testCase "Kettőshangzó" $
                idoSzotagol "Akhilleusz" @?= ["Akh", "ill", "e", "usz"]
            , testCase "Több szó" $
                idoSzotagol "réti virágok" @?= ["rét", "iv", "ir", "ág", "ok"]
            ]
        ]
    ]
