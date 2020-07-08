module Test.Validator.Named exposing (..)

import Dict
import Expect
import Test exposing (..)
import Validator.Named exposing (validate)
import Validator.String exposing (notEmpty)


hasNumberTest : Test
hasNumberTest =
    describe "Named validator"
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
                        Dict.fromList
                            [ ( "fieldA", [ "a is required" ] )
                            , ( "fieldB", [] )
                            ]
                in
                Expect.equal validated (Err errors)
            )
        ]
