module Validation.ValidatorTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Validation.Validator exposing (hasLetter, hasNumber)


hasNumberTest : Test
hasNumberTest =
    describe "hasNumber"
        [ test "succeeds"
            (\_ -> Expect.equal (hasNumber "error" "a1bc") (Ok "a1bc"))
        , test "fails"
            (\_ -> Expect.equal (hasNumber "error" "abc") (Err [ "error" ]))
        ]


hasLetterTest : Test
hasLetterTest =
    describe "hasLetter"
        [ test "succeeds"
            (\_ -> Expect.equal (hasLetter "error" "1a23") (Ok "1a23"))
        , test "fails"
            (\_ -> Expect.equal (hasLetter "error" "123") (Err [ "error" ]))
        ]
