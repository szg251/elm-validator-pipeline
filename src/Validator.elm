module Validator exposing
    ( Validated, Validator
    , noCheck, validate, checkOnly, validateMany, validateAll
    , map, andThen, mapErrors, many, all, customValidator
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
        |> checkOnly (Bool.isTrue "you need to approve") form.approved

Errors will be accumulated from top to bottom into a List. If you want to know exactly which field had
errors, take a look at the `Validator.Named` module.

@docs Validated, Validator


# Pipeline functions

@docs noCheck, validate, checkOnly, validateMany, validateAll


# Helpers

@docs map, andThen, mapErrors, many, all, customValidator

-}


{-| `Validated` is simply an alias for a Result type, with errors as a list of your type, which can be
a string, or any custom error type.
You will not need to work with some exotic type, this is only here for convenience.
-}
type alias Validated error value =
    Result (List error) value


{-| `Validator` is function, that checks a value, and returns a `Validated`. Some validators can change the type of the value of type `a` to type `b`.
-}
type alias Validator error a b =
    a -> Validated error b


{-| Apply a value to the pipeline without perfoming any checks.
-}
noCheck : a -> Validated x (a -> b) -> Validated x b
noCheck value applicative =
    case applicative of
        Ok toNext ->
            Ok (toNext value)

        Err errors ->
            Err errors


{-| Validate a value and apply it to the pipeline.
-}
validate : Validator x a b -> a -> Validated x (b -> c) -> Validated x c
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


{-| Validate a value without applying it to the pipeline.
-}
checkOnly : Validator x a b -> a -> Validated x c -> Validated x c
checkOnly validator value applicative =
    Result.andThen (always applicative) (validator value)


{-| Compose a list of validators for a single value. Checks are performed from left to right, and will stop on the first failure, returning only the first error.
-}
many : List (Validator x a a) -> Validator x a a
many =
    List.foldr (composeValidators Lazy) Ok


{-| Compose a list of validators for a single value. Checks are performed from left to right, and will return all errors.
-}
all : List (Validator x a a) -> Validator x a a
all =
    List.foldr (composeValidators Eager) Ok


{-| Validate a value using a list of validators. Checks are performed from left to right, and will stop on the first failure, returning only the first error.

Note: `validateMany` is a convenience function onver `validate << many`

-}
validateMany : List (Validator x a a) -> a -> Validated x (a -> b) -> Validated x b
validateMany =
    validate << many


{-| Validate a value using a list of validators. Checks are performed from left to right, and will return all errors.

Note: `validateAll` is a convenience function onver `validate << all`

-}
validateAll : List (Validator x a a) -> a -> Validated x (a -> b) -> Validated x b
validateAll =
    validate << all


{-| Create a custom validator, using a predicate function.
-}
customValidator : x -> (a -> Bool) -> Validator x a a
customValidator errorMsg predicate value =
    if predicate value then
        Ok value

    else
        Err [ errorMsg ]



-- Helpers


{-| Map a function into the happy path.
-}
map : (b -> c) -> Validator x a b -> Validator x a c
map fn validatorA value =
    validatorA value
        |> Result.map fn


{-| Chain together validators. Only the first error will be returned on failure.

Note: `validateMany` and `validateAll` provide a cleaner interface for most use cases.

-}
andThen : Validator x b c -> Validator x a b -> Validator x a c
andThen validatorB validatorA value =
    validatorA value
        |> Result.andThen validatorB


{-| Transfrom error values.
-}
mapErrors : (x -> y) -> Validator x a b -> Validator y a b
mapErrors fn validatorA value =
    validatorA value
        |> Result.mapError (List.map fn)


type ErrorEvaluation
    = Eager
    | Lazy


composeValidators : ErrorEvaluation -> Validator x a a -> Validator x a a -> Validator x a a
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
