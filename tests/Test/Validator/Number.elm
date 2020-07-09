module Test.Validator.Number exposing (..)

import Expect
import Test exposing (..)
import Validator.Number as Number


minTest : Test
minTest =
    describe "min"
        [ test "succeeds"
            (\_ -> Expect.equal (Number.min "error" 2 2) (Ok 2))
        , test "succeeds with float"
            (\_ -> Expect.equal (Number.min "error" 2 2.0) (Ok 2.0))
        , test "fails"
            (\_ -> Expect.equal (Number.min "error" 2 1) (Err [ "error" ]))
        ]


maxTest : Test
maxTest =
    describe "max"
        [ test "succeeds"
            (\_ -> Expect.equal (Number.max "error" 2 2) (Ok 2))
        , test "succeeds with float"
            (\_ -> Expect.equal (Number.max "error" 2 2.0) (Ok 2.0))
        , test "fails"
            (\_ -> Expect.equal (Number.max "error" 2 3) (Err [ "error" ]))
        ]
