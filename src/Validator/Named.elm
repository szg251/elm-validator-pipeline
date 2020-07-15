module Validator.Named exposing
    ( noCheck, validate, checkOnly, validateMany, validateAll
    , Validated, Errors, hasErrorsOn, getErrors, countErrors
    )

{-| Named validators work exactly the same way, as the simple ones, but every validate function takes a name string. Errors will be accumulated into a Dict, where the key is the given name, and the value is a
list of errors.

    Ok ValidatedForm
        |> validate "name" (notEmpty "name is required") form.name
        |> validate "email" (isEmail "email is invalid") form.email
        |> validateMany "password"
            [ hasLetter "password needs to have letters"
            , hasNumber "password needs to have numbers"
            ]
            form.password
        |> noCheck form.message
        |> checkOnly "approved" (Bool.isTrue "you need to approve") form.approved


# Pipeline functions

@docs noCheck, validate, checkOnly, validateMany, validateAll


# Errors

Named validators return lists of errors in a Dict, where the key is the field name. You don't even
need to use these helpers to get errors for a field, you can simply use `Dict.get NAME`.

@docs Validated, Errors, hasErrorsOn, getErrors, countErrors

-}

import Dict exposing (Dict)
import Validator exposing (Validator)


{-| `Validated` is an alias for a Result type with Errors.
-}
type alias Validated error value =
    Result (Errors error) value


{-| Named validators return lists of errors in a Dict, where the key is the field name.
-}
type alias Errors error =
    Dict String (List error)


{-| Pipe a value through without perfoming any checks.
-}
noCheck : a -> Validated x (a -> b) -> Validated x b
noCheck value =
    Validator.noCheck value |> ignoreErrors


{-| Validate a value using a validator.
-}
validate : String -> Validator x a b -> a -> Validated x (b -> c) -> Validated x c
validate fieldName validator value =
    Validator.validate validator value |> mapToNamedErrors fieldName


{-| Validate a value without applying it to the pipe.
-}
checkOnly : String -> Validator x a b -> a -> Validated x c -> Validated x c
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
validateMany : String -> List (Validator x a a) -> a -> Validated x (a -> b) -> Validated x b
validateMany fieldName validators value =
    Validator.validateMany validators value |> mapToNamedErrors fieldName


{-| Validate a value using a list of validators. Checks are performed from left to right, and will return all errors.
-}
validateAll : String -> List (Validator x a a) -> a -> Validated x (a -> b) -> Validated x b
validateAll fieldName validators value =
    Validator.validateAll validators value |> mapToNamedErrors fieldName


{-| Checks if there are any errors for a given field name.
-}
hasErrorsOn : String -> Validated x a -> Bool
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
getErrors : String -> Validated x a -> Maybe (List x)
getErrors fieldName validated =
    case validated of
        Ok _ ->
            Nothing

        Err errors ->
            Dict.get fieldName errors


{-| Count all the errors.
-}
countErrors : Validated x a -> Int
countErrors validated =
    case validated of
        Ok _ ->
            0

        Err errors ->
            Dict.values errors
                |> List.concat
                |> List.length


mapToNamedErrors :
    String
    -> (Validator.Validated x (b -> c) -> Validator.Validated x c)
    -> Validated x (b -> c)
    -> Validated x c
mapToNamedErrors fieldName validator applicative =
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
    -> Validated x (b -> c)
    -> Validated x c
ignoreErrors validator applicative =
    case applicative of
        Err errors ->
            validator (Err [])
                |> Result.mapError (always errors)

        Ok function ->
            validator (Ok function)
                |> Result.mapError (always Dict.empty)
