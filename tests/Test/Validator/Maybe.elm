module Test.Validator.Maybe exposing (..)

import Expect
import Test exposing (..)
import Validator.Maybe as Maybe
import Validator.String as String


isJustTest : Test
isJustTest =
    describe "isJust"
        [ test "succeeds"
            (\_ -> Expect.equal (Maybe.isJust "error" (Just "test")) (Ok "test"))
        , test "fails"
            (\_ -> Expect.equal (Maybe.isJust "error" Nothing) (Err [ "error" ]))
        ]


notRequiredTest : Test
notRequiredTest =
    describe "notRequired"
        [ test "succeeds"
            (\_ ->
                Expect.equal (Maybe.notRequired (String.letterOnly "error") (Just "test"))
                    (Ok (Just "test"))
            )
        , test "succeeds on Nothing"
            (\_ ->
                Expect.equal (Maybe.notRequired (String.letterOnly "error") Nothing)
                    (Ok Nothing)
            )
        , test "fails on invalid Just value"
            (\_ ->
                Expect.equal (Maybe.notRequired (String.letterOnly "error") (Just "test1"))
                    (Err [ "error" ])
            )
        ]
