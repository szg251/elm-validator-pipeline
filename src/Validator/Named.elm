module Validator.Named exposing
    ( errorCount
    , hasErrors
    , noCheck
    , validate
    , validateAll
    , validateMany
    )

import Dict exposing (Dict)
import String.Extra as StringE
import Validator exposing (Validator)


noCheck : a -> Result Errors (a -> b) -> Result Errors b
noCheck value =
    Validator.noCheck value |> ignoreErrors


validate : String -> Validator a b -> a -> Result Errors (b -> c) -> Result Errors c
validate fieldName validator value =
    Validator.validate validator value |> mapErrors fieldName


validateMany : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateMany fieldName validators value =
    Validator.validateMany validators value |> mapErrors fieldName


validateAll : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateAll fieldName validators value =
    Validator.validateAll validators value |> mapErrors fieldName


type alias Errors =
    Dict String (List String)


hasErrors : String -> Errors -> Bool
hasErrors fieldName errors =
    case Dict.get fieldName errors of
        Nothing ->
            False

        Just [] ->
            False

        Just _ ->
            True


errorCount : Errors -> Int
errorCount =
    Dict.values
        >> List.concat
        >> List.filter (not << StringE.isBlank)
        >> List.length


mapErrors :
    String
    ->
        (Result (List String) (b -> c)
         -> Result (List String) c
        )
    -> Result Errors (b -> c)
    -> Result Errors c
mapErrors fieldName validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError (\list -> Dict.insert fieldName list errors)

        Ok function ->
            validator (Ok function)
                |> Result.mapError (\list -> Dict.insert fieldName list Dict.empty)


ignoreErrors :
    (Result (List String) (b -> c)
     -> Result (List String) c
    )
    -> Result Errors (b -> c)
    -> Result Errors c
ignoreErrors validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError (always errors)

        Ok function ->
            validator (Ok function)
                |> Result.mapError (always Dict.empty)
