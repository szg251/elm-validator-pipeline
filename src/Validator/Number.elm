module Validator.Number exposing (min, max)

{-| Validators for Ints and Floats.

@docs min, max

-}

import Validator exposing (Validator, customValidator)


{-| Checks if a number is greater than or equal to a given number
-}
min : String -> number -> Validator number number
min errorMsg value =
    customValidator errorMsg (\tested -> tested >= value)


{-| Checks if a number is smaller than or equal to a given number
-}
max : String -> number -> Validator number number
max errorMsg value =
    customValidator errorMsg (\tested -> tested <= value)
