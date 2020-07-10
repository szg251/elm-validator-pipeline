module Validator.List exposing (minLength, maxLength, every)

{-| Validators for Lists.

@docs minLength, maxLength, every

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


{-| Checks if every item in a list passes the validation.
-}
every : Validator a a -> Validator (List a) (List a)
every validator =
    let
        compose =
            \item list ->
                Result.map2
                    (::)
                    (validator item)
                    list
    in
    List.foldr compose (Ok [])
