module Validator.List exposing (minLength, maxLength)

{-| Validators for Lists.

@docs minLength, maxLength

-}

import Validator exposing (Validator, customValidator)


{-| Checks if a list is longer than or equal to a given number
-}
minLength : String -> Int -> Validator (List a) (List a)
minLength errorMsg value =
    customValidator errorMsg (\tested -> List.length tested >= value)


{-| Checks if a list is shorter than or equal to a given number
-}
maxLength : String -> Int -> Validator (List a) (List a)
maxLength errorMsg value =
    customValidator errorMsg (\tested -> List.length tested <= value)
