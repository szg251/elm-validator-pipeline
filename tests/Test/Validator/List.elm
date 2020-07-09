module Test.Validator.List exposing (..)

import Expect
import Test exposing (..)
import Validator.List as List


minTest : Test
minTest =
    describe "min"
        [ test "succeeds"
            (\_ -> Expect.equal (List.minLength "error" 2 [ 0, 1 ]) (Ok [ 0, 1 ]))
        , test "fails"
            (\_ -> Expect.equal (List.minLength "error" 2 [ 0 ]) (Err [ "error" ]))
        ]


maxTest : Test
maxTest =
    describe "max"
        [ test "succeeds"
            (\_ -> Expect.equal (List.maxLength "error" 2 [ 0, 1 ]) (Ok [ 0, 1 ]))
        , test "fails"
            (\_ -> Expect.equal (List.maxLength "error" 2 [ 0, 1, 2 ]) (Err [ "error" ]))
        ]
