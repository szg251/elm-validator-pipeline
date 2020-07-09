module Validator exposing
    ( Validator
    , andThen
    , customValidator
    , map
    , noCheck
    , validate
    , validateAll
    , validateMany
    )


type alias Validated a =
    Result (List String) a


type alias Validator a b =
    a -> Validated b


noCheck : a -> Validated (a -> b) -> Validated b
noCheck value applicative =
    case applicative of
        Ok toNext ->
            Ok (toNext value)

        Err errors ->
            Err errors


validate : Validator a b -> a -> Validated (b -> c) -> Validated c
validate validator value applicative =
    let
        composeResults func ra rb =
            case ( ra, rb ) of
                ( Ok a, Ok b ) ->
                    Ok (func a b)

                ( Err x, Err y ) ->
                    Err (x ++ y)

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err err ) ->
                    Err err
    in
    composeResults (\toB a -> toB a) applicative (validator value)


validateMany : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateMany validators =
    validate (List.foldr (composeValidators Lazy) Ok validators)


validateAll : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateAll validators =
    validate (List.foldr (composeValidators Eager) Ok validators)


customValidator : String -> (a -> Bool) -> Validator a a
customValidator errorMsg predicate value =
    if predicate value then
        Ok value

    else
        Err [ errorMsg ]



-- Helpers


map : (b -> c) -> Validator a b -> Validator a c
map fn validatorA value =
    validatorA value
        |> Result.map fn


andThen : Validator b c -> Validator a b -> Validator a c
andThen validatorB validatorA value =
    validatorA value
        |> Result.andThen validatorB


type ErrorEvaluation
    = Eager
    | Lazy


composeValidators : ErrorEvaluation -> Validator a a -> Validator a a -> Validator a a
composeValidators errorEvaluation validatorA validatorB value =
    case errorEvaluation of
        Eager ->
            case ( validatorA value, validatorB value ) of
                ( Ok _, Ok _ ) ->
                    Ok value

                ( Err x, Err y ) ->
                    Err (x ++ y)

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err err ) ->
                    Err err

        Lazy ->
            case validatorA value of
                Err err ->
                    Err err

                Ok _ ->
                    validatorB value
