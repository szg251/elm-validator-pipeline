module Validation.Validator exposing (Validator, andThen, hasLetter, hasNumber, isEmail, isInt, isJust, isPhone, isUrl, letterOnly, map, maxLength, maxList, maxVal, minLength, minList, minVal, noCheck, notEmpty, notRequired, numberOnly, validate, validateAll, validateMany)

import Regex


type alias Validated a =
    Result (List String) a


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
    composeResults (\toB a -> toB a) (++) applicative (validator value)


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


isPhone : String -> Validator String String
isPhone errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(0([1-9]{1}-?[1-9]\\d{3}|[1-9]{2}-?\\d{3}|[1-9]{2}\\d{1}-?\\d{2}|[1-9]{2}\\d{2}-?\\d{1})-?\\d{4}|0[789]0-?\\d{4}-?\\d{4}|050-?\\d{4}-?\\d{4})$"
        )



-- Regex from https://gist.github.com/dperini/729294


isUrl : String -> Validator String String
isUrl errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(?:(?:(?:https?|ftp):)?\\/\\/)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z0-9\\u00a1-\\uffff][a-z0-9\\u00a1-\\uffff_-]{0,62})?[a-z0-9\\u00a1-\\uffff]\\.)+(?:[a-z\\u00a1-\\uffff]{2,}\\.?))(?::\\d{2,5})?(?:[/?#]\\S*)?$"
        )


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



-- List validators


minList : String -> Int -> Validator (List a) (List a)
minList errorMsg value =
    customValidator errorMsg (\tested -> List.length tested >= value)


maxList : String -> Int -> Validator (List a) (List a)
maxList errorMsg value =
    customValidator errorMsg (\tested -> List.length tested <= value)



-- Other validators


isJust : String -> Validator (Maybe a) a
isJust errorMsg maybeValue =
    case maybeValue of
        Nothing ->
            Err [ errorMsg ]

        Just value ->
            Ok value


isInt : String -> Validator String Int
isInt errorMsg value =
    case String.toInt value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int



-- Helpers


notRequired : Validator a a -> Validator (Maybe a) (Maybe a)
notRequired validator maybeValue =
    case maybeValue of
        Nothing ->
            Ok Nothing

        Just value ->
            Result.map Just (validator value)


andThen : Validator b c -> Validator a b -> Validator a c
andThen validatorB validatorA value =
    validatorA value
        |> Result.andThen validatorB


map : (b -> c) -> Validator a b -> Validator a c
map fn validatorA value =
    validatorA value
        |> Result.map fn


composeResults : (a -> b -> value) -> (x -> x -> x) -> Result x a -> Result x b -> Result x value
composeResults func errFunc ra rb =
    case ( ra, rb ) of
        ( Ok a, Ok b ) ->
            Ok (func a b)

        ( Err x, Err y ) ->
            Err (errFunc x y)

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err


type ErrorEvaluation
    = Eager
    | Lazy


composeValidators : ErrorEvaluation -> Validator a a -> Validator a a -> Validator a a
composeValidators errorEvaluation validatorA validatorB value =
    composeResults (\_ _ -> value)
        (\prevErrors nextErrors ->
            if errorEvaluation == Eager then
                prevErrors ++ nextErrors

            else
                prevErrors
        )
        (validatorA value)
        (validatorB value)
