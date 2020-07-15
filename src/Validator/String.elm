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
notEmpty : x -> Validator x String String
notEmpty errorMsg =
    customValidator errorMsg ((/=) "")


{-| Checks if a string is empty (white spaces are not allowed).
-}
notBlank : x -> Validator x String String
notBlank errorMsg =
    regexValidator errorMsg (Regex.fromString "[^\\s]")


{-| Checks if a string is valid email.
This validator works for most emails, but it is not 100%.

Regex is from: <https://emailregex.com/>

-}
isEmail : x -> Validator x String String
isEmail errorMsg =
    regexValidator errorMsg (Regex.fromString "^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$")


{-| Checks if a string is a valid Japanese phone number.

Note: I only added japanese regex, because that's what I use, but you can easily use your own regex, or send me a Pull Request.

-}
isPhoneJp : x -> Validator x String String
isPhoneJp errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(0([1-9]{1}-?[1-9]\\d{3}|[1-9]{2}-?\\d{3}|[1-9]{2}\\d{1}-?\\d{2}|[1-9]{2}\\d{2}-?\\d{1})-?\\d{4}|0[789]0-?\\d{4}-?\\d{4}|050-?\\d{4}-?\\d{4})$"
        )


{-| Checks if a string is valid URL.

Regex is from: <https://gist.github.com/dperini/729294>

-}
isUrl : x -> Validator x String String
isUrl errorMsg =
    regexValidator errorMsg
        (Regex.fromString
            "^(?:(?:(?:https?|ftp):)?\\/\\/)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z0-9\\u00a1-\\uffff][a-z0-9\\u00a1-\\uffff_-]{0,62})?[a-z0-9\\u00a1-\\uffff]\\.)+(?:[a-z\\u00a1-\\uffff]{2,}\\.?))(?::\\d{2,5})?(?:[/?#]\\S*)?$"
        )


{-| Checks if a string only contains letters (white spaces are not allowed).
-}
letterOnly : x -> Validator x String String
letterOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^[a-zA-Z]*$")


{-| Checks if a string contains at least one letter.
-}
hasLetter : x -> Validator x String String
hasLetter errorMsg =
    regexValidator errorMsg (Regex.fromString "[a-zA-Z]+")


{-| Checks if a string only contains numbers.
-}
numberOnly : x -> Validator x String String
numberOnly errorMsg =
    regexValidator errorMsg (Regex.fromString "^\\d*$")


{-| Checks if a string contains at least one number.
-}
hasNumber : x -> Validator x String String
hasNumber errorMsg =
    regexValidator errorMsg (Regex.fromString "\\d+")


{-| Checks if a string is longer than or equal to a given value.
-}
minLength : x -> Int -> Validator x String String
minLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested >= length)


{-| Checks if a string is shorter than or equal to a given value.
-}
maxLength : x -> Int -> Validator x String String
maxLength errorMsg length =
    customValidator errorMsg (\tested -> String.length tested <= length)


{-| Custom string validator using a regular expression.
-}
regexValidator : x -> Maybe Regex.Regex -> Validator x String String
regexValidator errorMsg regex value =
    customValidator errorMsg (Regex.contains (Maybe.withDefault Regex.never regex)) value



-- Casting validators


{-| Checks if a string can be casted to an integer, and if so, it returns the value.
-}
isInt : x -> Validator x String Int
isInt errorMsg value =
    case String.toInt value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int


{-| Checks if a string can be casted to a float and if so, it returns the value.
-}
isFloat : x -> Validator x String Float
isFloat errorMsg value =
    case String.toFloat value of
        Nothing ->
            Err [ errorMsg ]

        Just int ->
            Ok int
