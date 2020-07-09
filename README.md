# Validator Pipeline

The Validator Pipeline is based on the idea of **validator as a smart constructor**.

In Elm we try to specify our types as narrow as possible, to avoid invalid states. However, even with types, we could only specify if something is an `Int` and we cannot know, if it is positive or negative, or if something is a `String`, but we don't know if it is a valid URL. That's what we use validators for.

With this library, we can mix the two worlds: using a validator to create a unique type, and making it the only possible route to build that type. Here's an example:

```elm
module Price exposing (create, toInt)

import Validator.Int
import Validator (validate)

-- The constructor of Price is not exposed (it is an opaque type)
type Price = Price Int


create : Int -> Result (List String) -> Price
create value =
    Ok Price
        |> validate (Validator.Int.min "Price should be a positive integer." 0) value


toInt : Price -> Int
toInt (Price price) =
    price

```
