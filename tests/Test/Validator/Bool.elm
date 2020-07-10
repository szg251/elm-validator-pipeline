module Test.Validator.Bool exposing (..)

import Expect
import Test exposing (..)
import Validator.Bool as Bool


isTrueTest : Test
isTrueTest =
    describe "isTrue"
        [ test "succeeds"
            (\_ -> Expect.equal (Bool.isTrue "error" True) (Ok True))
        , test "fails"
            (\_ -> Expect.equal (Bool.isTrue "error" False) (Err [ "error" ]))
        ]


isFalseTest : Test
isFalseTest =
    describe "isFalse"
        [ test "succeeds"
            (\_ -> Expect.equal (Bool.isFalse "error" False) (Ok False))
        , test "fails"
            (\_ -> Expect.equal (Bool.isFalse "error" True) (Err [ "error" ]))
        ]
