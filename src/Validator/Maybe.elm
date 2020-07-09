module Validator.Maybe exposing (isJust, notRequired)

{-| Validators and helpers for Maybes.

@docs isJust, notRequired

-}

import Validator exposing (Validator)


{-| Checks if a Maybe has a value, and if so, returns it.
-}
isJust : String -> Validator (Maybe a) a
isJust errorMsg maybeValue =
    case maybeValue of
        Nothing ->
            Err [ errorMsg ]

        Just value ->
            Ok value


{-| This helper can be used to check the correctness of a non required Maybe.
It will only pass if the Maybe is Nothing, or if the value of Just passes the given validator.

    notRequired (Validator.String.isEmail "This is not a valid email") Nothing == Ok Nothing

    notRequired (Validator.String.isEmail "This is not a valid email") (Just "test@example.com") == Ok (Just "test@example.com")

    notRequired (Validator.String.isEmail "This is not a valid email") (Just "test") == Err [ "This is not a valid email" ]

-}
notRequired : Validator a a -> Validator (Maybe a) (Maybe a)
notRequired validator maybeValue =
    case maybeValue of
        Nothing ->
            Ok Nothing

        Just value ->
            Result.map Just (validator value)
