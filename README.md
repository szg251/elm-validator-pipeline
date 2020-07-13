# Validator Pipeline

The Validator Pipeline is based on the idea of **validator as a smart constructor**.

In Elm we try to specify our types as narrow as possible, to avoid invalid states. However, even with types, we could only specify if something is an `Int` and we cannot know, if it is positive or negative, or if something is a `String`, but we don't know if it is a valid URL. That's what we use validators for.

With this library, we can mix the two worlds: using a validator to create a unique type, and making it the only possible route to build that type, so we can know for sure, that the type represents a valid value. Here's an example:

```elm
module Price exposing (create, toInt)

import Validator.Int
import Validator exposing (validate)

-- The constructor of Price is not exposed (it is an opaque type)
type Price = Price Int


create : Int -> Result (List String) -> Result (List String) Price
create value =
    validate (Validator.Int.min "Price should be a positive number." 0) value
        |> Result.map Price


toInt : Price -> Int
toInt (Price price) =
    price

```

Of course, most of the time, we have less trivial data types, for example records.
For thoose, we can use the pipeline syntax, which might be similar to most us the folks from the
json-decode-pipeline package. Values are checked, and applied one by one a function.

In the example below, we use the pipeline functions from the `Validator.Named` module, which collects errors
into a Dict, so we can display errors for each field.

```elm
type alias Form =
    { name : String
    , email : String
    , age : String
    , password : String
    , message : String
    , approved : Bool
    }


type alias ValidForm =
    { name : String
    , email : String
    , age : Int
    , password : String
    , message : String
    }


validate : Form -> Validated ValidForm
validate form =
    Ok ValidForm
        |> validate "name" (Validator.String.notEmpty "name is required") form.name
        |> validate "email" (Validator.String.isEmail "email is invalid") form.email
        |> validate "age" (Validator.String.isInt "age is not a number") form.age
        |> validateMany "password"
            [ Validator.String.hasLetter "password needs to have letters"
            , Validator.String.hasNumber "password needs to have numbers"
            ]
            form.password
        |> noCheck form.message
        |> checkOnly "approved" (Validator.Bool.isTrue "you need to approve") model.approved

```

You can run this validation in the update function, and do something with it depending on the result.

```elm
postForm : ValidForm -> Cmd Msg
postForm validForm =
    -- postForm only accepts validated forms
    ...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submitted ->
            let
                validated =
                    validate model.form
            in
            -- We save the validation result, so we can display errors in the view function
            ( { model | validated = validated }
            , case validated of
                Err _ ->
                    Cmd.none

                Ok validForm ->
                    postForm validForm
            )
```

And display the errors in the view.

```elm
view : Model -> Html Msg
view model =
    ...
    form [ onSubmit Submitted ]
        [ input [ onInput InputName, value model.name ] []
        , viewErrors "name" model.validated
        ...
        ]



viewErrors : String -> Validated ValidForm -> Html Msg
viewErrors fieldName validated =
    case Validator.Named.getErrors "name" model.validated of
        Nothing ->
            text ""

        Just errors ->
            div [] (List.map (\error -> div [] [ text error ]) errors)
```

See the whole example here: https://ellie-app.com/9p8DhrVHhRQa1
