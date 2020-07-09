module Validator.Number exposing (max, min)

import Validator exposing (Validator, customValidator)


min : String -> number -> Validator number number
min errorMsg value =
    customValidator errorMsg (\tested -> tested >= value)


max : String -> number -> Validator number number
max errorMsg value =
    customValidator errorMsg (\tested -> tested <= value)
