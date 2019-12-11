module Validation.Named exposing (..)

import Dict exposing (Dict)
import Validation.Validator as Validator exposing (Validator)



-- Error mapping


type alias Errors =
    Dict String (List String)


mapErrors : String -> (Result (List String) (b -> c) -> Result (List String) c) -> Result Errors (b -> c) -> Result Errors c
mapErrors fieldName validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError (\list -> Dict.insert fieldName list errors)

        Ok function ->
            validator (Ok function)
                |> Result.mapError (\list -> Dict.insert fieldName list Dict.empty)


ignoreErrors : (Result (List String) (b -> c) -> Result (List String) c) -> Result Errors (b -> c) -> Result Errors c
ignoreErrors validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError (always errors)

        Ok function ->
            validator (Ok function)
                |> Result.mapError (always Dict.empty)


validate : String -> Validator a b -> a -> Result Errors (b -> c) -> Result Errors c
validate fieldName validator value =
    Validator.validate validator value |> mapErrors fieldName


noCheck : a -> Result Errors (a -> b) -> Result Errors b
noCheck value =
    Validator.noCheck value |> ignoreErrors


validateMany : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateMany fieldName validators value =
    Validator.validateMany validators value |> mapErrors fieldName


validateAll : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateAll fieldName validators value =
    Validator.validateAll validators value |> mapErrors fieldName
