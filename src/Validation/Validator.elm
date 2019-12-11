module Validation.Validator exposing (..)

import Regex

type alias Validated a = Result (List String) a

type alias Validator a b =
    a -> Validated b


noCheck : a -> Validated (a -> b) -> Validated b
noCheck value applicative =
    case applicative of
        Ok toNext ->
            Ok (toNext value)

        Err errors ->
            Err errors


validate : Validator a b -> a -> Validated (b -> c) -> Validated c
validate validator value applicative =
    case ( applicative, validator value ) of
        ( Ok toB, Ok a ) ->
            Ok (toB a)

        ( Err prevErrors, Err nextError ) ->
            Err (prevErrors ++ nextError)

        ( Err prevErrors, _ ) ->
            Err prevErrors

        ( _, Err nextError ) ->
            Err nextError


type ErrorEvaluation
    = Eager
    | Lazy


composeValidators : ErrorEvaluation -> Validator a a -> Validator a a -> Validator a a
composeValidators errorEvaluation validatorA validatorB value =
    case ( validatorA value, validatorB value ) of
        ( Ok _, Ok _ ) ->
            Ok value

        ( Err prevErrors, Err nextErrors ) ->
            if errorEvaluation == Eager then
                Err (prevErrors ++ nextErrors)

            else
                Err prevErrors

        ( Err prevErrors, _ ) ->
            Err prevErrors

        ( _, Err nextErrors ) ->
            Err nextErrors



-- composeValidators : ErrorEvaluation -> Validator a a -> Validator a a -> Validator a a
-- composeValidators errorEvaluation validatorA validatorB value =
--     case validatorA value of
--         Ok _ ->
--             validatorB value
--         Err errorsA ->
--             if errorEvaluation == Lazy then
--                 Err errorsA
--             else
--                 Result.mapError (\errorsB -> errorsA ++ errorsB) (validatorB value)


validateMany : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateMany validators =
    validate (List.foldl (composeValidators Lazy) Ok validators)


validateAll : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateAll validators =
    validate (List.foldl (composeValidators Eager) Ok validators)



-- Validators


customValidator : String -> (a -> Bool) -> Validator a a
customValidator errorMsg predicate value =
    if predicate value then
        Ok value

    else
        Err [ errorMsg ]



-- String validators


regexValidator : String -> Maybe Regex.Regex -> Validator String String
regexValidator errorMsg regex value =
    customValidator errorMsg (Regex.contains (Maybe.withDefault Regex.never regex)) value


isEmail : String -> Validator String String
isEmail errorMsg =
    regexValidator errorMsg (Regex.fromString "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$")


notEmpty : String -> Validator String String
notEmpty errorMsg =
    customValidator errorMsg ((/=) "")


letterOnly : String -> Validator String String
letterOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^[a-zA-Z]*$")


hasLetter : String -> Validator String String
hasLetter errorMsg =
    regexValidator errorMsg (Regex.fromString "[a-zA-Z]+")


numberOnly : String -> Validator String String
numberOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^[0-9]*$")


hasNumber : String -> Validator String String
hasNumber errorMsg =
    regexValidator errorMsg (Regex.fromString "[0-9]+")


minLength : String -> Int -> Validator String String
minLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested >= length)


maxLength : String -> Int -> Validator String String
maxLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested <= length)



-- Int validators


minVal : String -> Int -> Validator Int Int
minVal errorMsg value =
    customValidator errorMsg (\tested -> tested >= value)


maxVal : String -> Int -> Validator Int Int
maxVal errorMsg value =
    customValidator errorMsg (\tested -> tested <= value)



-- Other validators


isJust : String -> Validator (Maybe a) a
isJust errorMsg maybeValue =
    case maybeValue of
        Nothing ->
            Err [ errorMsg ]

        Just value ->
            Ok value
