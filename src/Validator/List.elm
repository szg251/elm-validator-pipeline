module Validator.List exposing (max, min)

import Validator exposing (Validator, customValidator)


min : String -> Int -> Validator (List a) (List a)
min errorMsg value =
    customValidator errorMsg (\tested -> List.length tested >= value)


max : String -> Int -> Validator (List a) (List a)
max errorMsg value =
    customValidator errorMsg (\tested -> List.length tested <= value)
