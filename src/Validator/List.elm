module Validator.List exposing (maxLength, minLength)

import Validator exposing (Validator, customValidator)


minLength : String -> Int -> Validator (List a) (List a)
minLength errorMsg value =
    customValidator errorMsg (\tested -> List.length tested >= value)


maxLength : String -> Int -> Validator (List a) (List a)
maxLength errorMsg value =
    customValidator errorMsg (\tested -> List.length tested <= value)
