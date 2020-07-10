module Test.Validator exposing (..)

import Expect
import Test exposing (..)
import Validator exposing (checkOnly, noCheck, validate, validateAll, validateMany)
import Validator.String exposing (letterOnly, notBlank, notEmpty)


validateTest : Test
validateTest =
    describe "validate"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") "data a"
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data a", "data b" ))
            )
        , test "fails"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") ""
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Err [ "a is required" ])
            )
        , test "fails on multiple fields"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") ""
                            |> validate (notEmpty "b is required") ""
                in
                Expect.equal validated (Err [ "a is required", "b is required" ])
            )
        ]


noCheckTest : Test
noCheckTest =
    describe "noCheck"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") "data a"
                            |> noCheck "data b"
                in
                Expect.equal validated (Ok ( "data a", "data b" ))
            )
        ]


checkOnlyTest : Test
checkOnlyTest =
    describe "checkOnly"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") "data a"
                            |> checkOnly (notEmpty "c is required") "data b"
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data a", "data b" ))
            )
        , test "fails"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate (notEmpty "a is required") "data a"
                            |> checkOnly (notEmpty "c is required") ""
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Err [ "c is required" ])
            )
        ]


validateManyTest : Test
validateManyTest =
    describe "validateMany"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateMany
                                [ notEmpty "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "data"
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data", "data b" ))
            )
        , test "fails on the first validator, and returns its error"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateMany
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Err [ "a is required" ])
            )
        ]


validateAllTest : Test
validateAllTest =
    describe "validateAll"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll
                                [ notEmpty "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "data"
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data", "data b" ))
            )
        , test "fails, and returns both validation errors"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Err [ "a is required", "only letters allowed" ])
            )
        ]
