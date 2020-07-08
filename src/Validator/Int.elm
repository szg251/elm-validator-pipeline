module Validator.Int exposing (max, min)

import Validator exposing (Validator, customValidator)


min : String -> Int -> Validator Int Int
min errorMsg value =
    customValidator errorMsg (\tested -> tested >= value)


max : String -> Int -> Validator Int Int
max errorMsg value =
    customValidator errorMsg (\tested -> tested <= value)
