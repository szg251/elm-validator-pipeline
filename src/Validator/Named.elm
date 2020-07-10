module Validator.Named exposing
    ( noCheck, validate, checkOnly, validateMany, validateAll
    , Validated, Errors, hasErrorsOn, getErrors, countErrors
    )

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
        |> checkOnly "approved" Bool.isTrue form.approved

Errors will be accumulated from top to bottom into a Dict, where the key is the field name.


# Pipeline functions

@docs noCheck, validate, checkOnly, validateMany, validateAll


# Errors

Named validators return lists of errors in a Dict, where the key is the field name. You don't even
need to use these helpers to get errors for a field, you can simply use `Dict.get FIELDNAME`.

@docs Validated, Errors, hasErrorsOn, getErrors, countErrors

-}

import Dict exposing (Dict)
import Validator exposing (Validator)


{-| `Validated` is an alias for a Result type with Errors.
-}
type alias Validated a =
    Result Errors a


{-| Named validators return lists of errors in a Dict, where the key is the field name.
-}
type alias Errors =
    Dict String (List String)


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


{-| Validate a value without applying it to the pipe.
-}
checkOnly : String -> Validator a b -> a -> Validated c -> Validated c
checkOnly fieldName validator value applicative =
    case applicative of
        Err errors ->
            validator value
                |> Result.mapError
                    (\list ->
                        if list == [] then
                            errors

                        else
                            Dict.insert fieldName list errors
                    )
                |> Result.andThen (always applicative)

        Ok _ ->
            validator value
                |> Result.mapError (\list -> Dict.insert fieldName list Dict.empty)
                |> Result.andThen (always applicative)


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


{-| Checks if there are any errors for a given field name.
-}
hasErrorsOn : String -> Validated a -> Bool
hasErrorsOn fieldName validated =
    case validated of
        Ok _ ->
            False

        Err errors ->
            case Dict.get fieldName errors of
                Nothing ->
                    False

                Just [] ->
                    False

                Just _ ->
                    True


{-| Get errors for a given field.
-}
getErrors : String -> Validated a -> Maybe (List String)
getErrors fieldName validated =
    case validated of
        Ok _ ->
            Nothing

        Err errors ->
            Dict.get fieldName errors


{-| Count all the errors.
-}
countErrors : Validated a -> Int
countErrors validated =
    case validated of
        Ok _ ->
            0

        Err errors ->
            Dict.values errors
                |> List.concat
                |> List.filter (not << (==) "")
                |> List.length


mapErrors :
    String
    -> (Validator.Validated (b -> c) -> Validator.Validated c)
    -> Result Errors (b -> c)
    -> Result Errors c
mapErrors fieldName validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError
                    (\list ->
                        if list == [] then
                            errors

                        else
                            Dict.insert fieldName list errors
                    )

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
