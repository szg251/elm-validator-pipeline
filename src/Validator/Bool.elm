module Validator.Bool exposing (isTrue, isFalse)

{-| Validators for Bools.

@docs isTrue, isFalse

-}

import Validator exposing (Validator, customValidator)


{-| Checks if a boolean is true
-}
isTrue : String -> Validator Bool Bool
isTrue errorMsg =
    customValidator errorMsg identity


{-| Checks if a boolean is false
-}
isFalse : String -> Validator Bool Bool
isFalse errorMsg =
    customValidator errorMsg not
