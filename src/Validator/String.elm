module Validator.String exposing
    ( notEmpty, notBlank, isEmail, isPhoneJp, isUrl, hasLetter, letterOnly, hasNumber, numberOnly, minLength, maxLength, regexValidator
    , isInt, isFloat
    )

{-| Validators for strings.


# Validators

@docs notEmpty, notBlank, isEmail, isPhoneJp, isUrl, hasLetter, letterOnly, hasNumber, numberOnly, minLength, maxLength, regexValidator


# Casting validators

@docs isInt, isFloat

-}

import Regex
import Validator exposing (Validator, customValidator)


{-| Checks if a string is empty (white spaces allowed).
-}
notEmpty : String -> Validator String String
notEmpty errorMsg =
    customValidator errorMsg ((/=) "")


{-| Checks if a string is empty (white spaces are not allowed).
-}
notBlank : String -> Validator String String
notBlank errorMsg =
    regexValidator errorMsg (Regex.fromString "[^\\s]")


{-| Checks if a string is valid email.
This validator works for most emails, but it is not 100%.

Regex is from: <https://emailregex.com/>

-}
isEmail : String -> Validator String String
isEmail errorMsg =
    regexValidator errorMsg (Regex.fromString "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$")


{-| Checks if a string is a valid Japanese phone number.

Note: I only added japanese regex, because that's what I use, but you can easily use your own regex, or send me a Pull Request.

-}
isPhoneJp : String -> Validator String String
isPhoneJp errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(0([1-9]{1}-?[1-9]\\d{3}|[1-9]{2}-?\\d{3}|[1-9]{2}\\d{1}-?\\d{2}|[1-9]{2}\\d{2}-?\\d{1})-?\\d{4}|0[789]0-?\\d{4}-?\\d{4}|050-?\\d{4}-?\\d{4})$"
        )


{-| Checks if a string is valid URL.

Regex is from: <https://gist.github.com/dperini/729294>

-}
isUrl : String -> Validator String String
isUrl errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(?:(?:(?:https?|ftp):)?\\/\\/)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z0-9\\u00a1-\\uffff][a-z0-9\\u00a1-\\uffff_-]{0,62})?[a-z0-9\\u00a1-\\uffff]\\.)+(?:[a-z\\u00a1-\\uffff]{2,}\\.?))(?::\\d{2,5})?(?:[/?#]\\S*)?$"
        )


{-| Checks if a string only contains letters (white spaces are not allowed).
-}
letterOnly : String -> Validator String String
letterOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^[a-zA-Z]*$")


{-| Checks if a string contains at least one letter.
-}
hasLetter : String -> Validator String String
hasLetter errorMsg =
    regexValidator errorMsg (Regex.fromString "[a-zA-Z]+")


{-| Checks if a string only contains numbers.
-}
numberOnly : String -> Validator String String
numberOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^\\d*$")


{-| Checks if a string contains at least one number.
-}
hasNumber : String -> Validator String String
hasNumber errorMsg =
    regexValidator errorMsg (Regex.fromString "\\d+")


{-| Checks if a string is longer than or equal to a given value.
-}
minLength : String -> Int -> Validator String String
minLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested >= length)


{-| Checks if a string is shorter than or equal to a given value.
-}
maxLength : String -> Int -> Validator String String
maxLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested <= length)


{-| Custom string validator using a regular expression.
-}
regexValidator : String -> Maybe Regex.Regex -> Validator String String
regexValidator errorMsg regex value =
    customValidator errorMsg (Regex.contains (Maybe.withDefault Regex.never regex)) value



-- Casting validators


{-| Checks if a string can be casted to an integer, and if so, it returns the value.
-}
isInt : String -> Validator String Int
isInt errorMsg value =
    case String.toInt value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int


{-| Checks if a string can be casted to a float and if so, it returns the value.
-}
isFloat : String -> Validator String Float
isFloat errorMsg value =
    case String.toFloat value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int
