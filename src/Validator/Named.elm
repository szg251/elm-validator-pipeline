module Validator.Named exposing (noCheck, validate, validateMany, validateAll, hasErrors, errorCount)

{-| Named validators work exactly the same way, as the simple ones, but every validate function takes a name string.

    Ok ValidatedForm
        |> validate "name" (notEmpty "name is required") form.name
        |> validate "email" (isEmail "email is invalid") form.email
        |> validateMany "password"
            [ hasLetter "password needs to have letters"
            , hasNumber "password needs to have numbers"
            ]
            form.password
        |> noCheck form.message

Errors will be accumulated from top to bottom into a Dict, where the key is the field name.

@docs noCheck, validate, validateMany, validateAll, hasErrors, errorCount

-}

import Dict exposing (Dict)
import String.Extra as StringE
import Validator exposing (Validator)


{-| Pipe a value through without perfoming any checks.
-}
noCheck : a -> Result Errors (a -> b) -> Result Errors b
noCheck value =
    Validator.noCheck value |> ignoreErrors


{-| Validate a value using a validator.
-}
validate : String -> Validator a b -> a -> Result Errors (b -> c) -> Result Errors c
validate fieldName validator value =
    Validator.validate validator value |> mapErrors fieldName


{-| Validate a value using a list of validators. Checks are performed from left to right, and will stop on the first failure, returning only the first error.
-}
validateMany : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateMany fieldName validators value =
    Validator.validateMany validators value |> mapErrors fieldName


{-| Validate a value using a list of validators. Checks are performed from left to right, and will return all errors.
-}
validateAll : String -> List (Validator a a) -> a -> Result Errors (a -> b) -> Result Errors b
validateAll fieldName validators value =
    Validator.validateAll validators value |> mapErrors fieldName


{-| Named validators return lists of errors in a Dict, where the key is the field name.
-}
type alias Errors =
    Dict String (List String)


{-| Checks if there are any errors for a given field name
-}
hasErrors : String -> Errors -> Bool
hasErrors fieldName errors =
    case Dict.get fieldName errors of
        Nothing ->
            False

        Just [] ->
            False

        Just _ ->
            True


{-| Count all the errors
-}
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
