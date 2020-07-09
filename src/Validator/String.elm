module Validator.String exposing (hasLetter, hasNumber, isEmail, isFloat, isInt, isPhoneJp, isUrl, letterOnly, maxLength, minLength, notBlank, notEmpty, numberOnly)

import Regex
import Validator exposing (Validator, customValidator)


regexValidator : String -> Maybe Regex.Regex -> Validator String String
regexValidator errorMsg regex value =
    customValidator errorMsg (Regex.contains (Maybe.withDefault Regex.never regex)) value


isEmail : String -> Validator String String
isEmail errorMsg =
    regexValidator errorMsg (Regex.fromString "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$")


isPhoneJp : String -> Validator String String
isPhoneJp errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(0([1-9]{1}-?[1-9]\\d{3}|[1-9]{2}-?\\d{3}|[1-9]{2}\\d{1}-?\\d{2}|[1-9]{2}\\d{2}-?\\d{1})-?\\d{4}|0[789]0-?\\d{4}-?\\d{4}|050-?\\d{4}-?\\d{4})$"
        )


isUrl : String -> Validator String String
isUrl errorMsg =
    -- Regex from https://gist.github.com/dperini/729294
    regexValidator errorMsg
        (Regex.fromString
            "^(?:(?:(?:https?|ftp):)?\\/\\/)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z0-9\\u00a1-\\uffff][a-z0-9\\u00a1-\\uffff_-]{0,62})?[a-z0-9\\u00a1-\\uffff]\\.)+(?:[a-z\\u00a1-\\uffff]{2,}\\.?))(?::\\d{2,5})?(?:[/?#]\\S*)?$"
        )


notEmpty : String -> Validator String String
notEmpty errorMsg =
    customValidator errorMsg ((/=) "")


notBlank : String -> Validator String String
notBlank errorMsg =
    regexValidator errorMsg (Regex.fromString "[^\\s]")


letterOnly : String -> Validator String String
letterOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^[a-zA-Z]*$")


hasLetter : String -> Validator String String
hasLetter errorMsg =
    regexValidator errorMsg (Regex.fromString "[a-zA-Z]+")


numberOnly : String -> Validator String String
numberOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^\\d*$")


hasNumber : String -> Validator String String
hasNumber errorMsg =
    regexValidator errorMsg (Regex.fromString "\\d+")


minLength : String -> Int -> Validator String String
minLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested >= length)


maxLength : String -> Int -> Validator String String
maxLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested <= length)



-- Casting validators


isInt : String -> Validator String Int
isInt errorMsg value =
    case String.toInt value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int


isFloat : String -> Validator String Float
isFloat errorMsg value =
    case String.toFloat value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int
