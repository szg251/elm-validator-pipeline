module Validator exposing
    ( Validated, Validator, noCheck, validate, many, all, validateMany, validateAll, customValidator
    , map, andThen
    , checkOnly
    )

{-| Validators work in a pipeline (or applicative functor style), similar to the one used in
json-decode-pipeline. Values are checked, and applied one by one a function.
If everything goes well, the pipeline returns an `Ok` result, otherwise it will return all the errors.

    Ok ValidatedForm
        |> validate (String.notEmpty "name is required") form.name
        |> validate (String.isEmail "email is invalid") form.email
        |> validateMany
            [ String.hasLetter "password needs to have letters"
            , String.hasNumber "password needs to have numbers"
            ]
            form.password
        |> noCheck form.message
        |> checkOnly Bool.isTrue form.approved

Errors will be accumulated from top to bottom into a List. If you want to know exactly which field had
errors, take a look at the `Validator.Named` module.


# Core functions

@docs Validated, Validator, noCheck, validate, many, all, validateMany, validateAll, customValidator


# Helpers

@docs map, andThen

-}


{-| `Validated` is simply an alias for a Result type, with errors as a list of string.
You will not need to work with some exotic type, it is only here for convenience.
-}
type alias Validated a =
    Result (List String) a


{-| `Validator` is function, that checks a value, and returns a `Validated`. Some validators can change the type of the value, so the two type parameters are represent the input and the output types.
-}
type alias Validator a b =
    a -> Validated b


{-| Pipe a value through without perfoming any checks.
-}
noCheck : a -> Validated (a -> b) -> Validated b
noCheck value applicative =
    case applicative of
        Ok toNext ->
            Ok (toNext value)

        Err errors ->
            Err errors


{-| Validate a value using a validator.
-}
validate : Validator a b -> a -> Validated (b -> c) -> Validated c
validate validator value applicative =
    case ( applicative, validator value ) of
        ( Ok func, Ok validated ) ->
            Ok (func validated)

        ( Err x, Err y ) ->
            Err (x ++ y)

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err


{-| Validate a value without applying it to the pipe.
-}
checkOnly : Validator a b -> a -> Validated c -> Validated c
checkOnly validator value applicative =
    Result.andThen (always applicative) (validator value)


{-| Compose a list of validators for a single value. Checks are performed from left to right, and will stop on the first failure, returning only the first error.
-}
many : List (Validator a a) -> Validator a a
many =
    List.foldr (composeValidators Lazy) Ok


{-| Compose a list of validators for a single value. Checks are performed from left to right, and will return all errors.
-}
all : List (Validator a a) -> Validator a a
all =
    List.foldr (composeValidators Eager) Ok


{-| Validate a value using a list of validators. Checks are performed from left to right, and will stop on the first failure, returning only the first error.

Note: `validateMany` is a convenience function onver `validate << many`

-}
validateMany : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateMany =
    validate << many


{-| Validate a value using a list of validators. Checks are performed from left to right, and will return all errors.

Note: `validateAll` is a convenience function onver `validate << all`

-}
validateAll : List (Validator a a) -> a -> Validated (a -> b) -> Validated b
validateAll =
    validate << all


{-| Create a custom validator, using a predicate function.
-}
customValidator : String -> (a -> Bool) -> Validator a a
customValidator errorMsg predicate value =
    if predicate value then
        Ok value

    else
        Err [ errorMsg ]



-- Helpers


{-| Map a function into the happy path.
-}
map : (b -> c) -> Validator a b -> Validator a c
map fn validatorA value =
    validatorA value
        |> Result.map fn


{-| Chain together validators. Only the first error will be returned on failure.

Note: `validateMany` and `validateAll` provide a cleaner interface for most use cases.

-}
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
            validatorA value
                |> Result.andThen (\_ -> validatorB value)
