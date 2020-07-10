module Test.Validator.Named exposing (..)

import Dict
import Expect
import Test exposing (..)
import Validator.Named exposing (checkOnly, countErrors, hasErrorsOn, noCheck, validate, validateAll, validateMany)
import Validator.String exposing (letterOnly, notBlank, notEmpty)


noCheckTest : Test
noCheckTest =
    describe "noCheck"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate "fieldA" (notEmpty "a is required") "data a"
                            |> noCheck "data b"
                in
                Expect.equal validated (Ok ( "data a", "data b" ))
            )
        ]


validateTest : Test
validateTest =
    describe "validate"
        [ test "succeeds"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate "fieldA" (notEmpty "a is required") "data a"
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data a", "data b" ))
            )
        , test "fails"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate "fieldA" (notEmpty "a is required") ""
                            |> validate "fieldB" (notEmpty "b is required") "data b"

                    errors =
                        Dict.fromList [ ( "fieldA", [ "a is required" ] ) ]
                in
                Expect.equal validated (Err errors)
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
                            |> validate "fieldA" (notEmpty "a is required") "data a"
                            |> checkOnly "fieldB" (notEmpty "b is required") "data b"
                            |> validate "fieldC" (notEmpty "c is required") "data c"
                in
                Expect.equal validated (Ok ( "data a", "data c" ))
            )
        , test "fails"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validate "fieldA" (notEmpty "a is required") "data a"
                            |> checkOnly "fieldB" (notEmpty "b is required") ""
                            |> validate "fieldC" (notEmpty "c is required") ""

                    errors =
                        Dict.fromList
                            [ ( "fieldB", [ "b is required" ] )
                            , ( "fieldC", [ "c is required" ] )
                            ]
                in
                Expect.equal validated (Err errors)
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
                            |> validateMany "fieldA"
                                [ notEmpty "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "data"
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data", "data b" ))
            )
        , test "fails on the first validator, and returns its error"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateMany "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate "fieldB" (notEmpty "b is required") "data b"

                    errors =
                        Dict.fromList
                            [ ( "fieldA", [ "a is required" ] )
                            ]
                in
                Expect.equal validated (Err errors)
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
                            |> validateAll "fieldA"
                                [ notEmpty "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "data"
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal validated (Ok ( "data", "data b" ))
            )
        , test "fails, and returns both validation errors"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate "fieldB" (notEmpty "b is required") "data b"

                    errors =
                        Dict.fromList
                            [ ( "fieldA", [ "a is required", "only letters allowed" ] )
                            ]
                in
                Expect.equal validated (Err errors)
            )
        ]


errorHelpersTest : Test
errorHelpersTest =
    describe "named error helpers"
        [ test "hasErrorsOn is true"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal (hasErrorsOn "fieldA" validated) True
            )
        , test "hasErrorsOn is false"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "abc"
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal (hasErrorsOn "fieldA" validated) False
            )
        , test "count errors"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                " "
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal (countErrors validated) 2
            )
        , test "count errors on success"
            (\_ ->
                let
                    validated =
                        Ok Tuple.pair
                            |> validateAll "fieldA"
                                [ notBlank "a is required"
                                , letterOnly "only letters allowed"
                                ]
                                "abc"
                            |> validate "fieldB" (notEmpty "b is required") "data b"
                in
                Expect.equal (countErrors validated) 0
            )
        ]
