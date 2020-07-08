module Validator.Maybe exposing (isJust, notRequired)

import Validator exposing (Validator)


isJust : String -> Validator (Maybe a) a
isJust errorMsg maybeValue =
    case maybeValue of
        Nothing ->
            Err [ errorMsg ]

        Just value ->
            Ok value


notRequired : Validator a a -> Validator (Maybe a) (Maybe a)
notRequired validator maybeValue =
    case maybeValue of
        Nothing ->
            Ok Nothing

        Just value ->
            Result.map Just (validator value)
