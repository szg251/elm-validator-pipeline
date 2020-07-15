module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (checked, style, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Validator.Bool
import Validator.Named exposing (Validated, checkOnly, noCheck, validate, validateMany)
import Validator.String


type Msg
    = InputName String
    | InputEmail String
    | InputAge String
    | InputPassword String
    | InputMessage String
    | ToggleApproved Bool
    | Submitted


type alias Model =
    { name : String
    , email : String
    , age : String
    , password : String
    , message : String
    , approved : Bool
    , validated : Validated String ValidForm
    }


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , email = ""
      , age = ""
      , password = ""
      , message = ""
      , approved = False
      , validated = Err Dict.empty
      }
    , Cmd.none
    )


type alias ValidForm =
    { name : String
    , email : String
    , age : Int
    , password : String
    , message : String
    }


validateForm : Model -> Validated String ValidForm
validateForm model =
    Ok ValidForm
        |> validate "name" (Validator.String.notEmpty "name is required") model.name
        |> validate "email" (Validator.String.isEmail "email is invalid") model.email
        |> validate "age" (Validator.String.isInt "age is not a number") model.age
        |> validateMany "password"
            [ Validator.String.hasLetter "password needs to contain letters"
            , Validator.String.hasNumber "password needs to contain numbers"
            ]
            model.password
        |> noCheck model.message
        |> checkOnly "approved" (Validator.Bool.isTrue "please approve") model.approved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputName name ->
            ( { model | name = name }, Cmd.none )

        InputEmail email ->
            ( { model | email = email }, Cmd.none )

        InputAge age ->
            ( { model | age = age }, Cmd.none )

        InputPassword password ->
            ( { model | password = password }, Cmd.none )

        InputMessage message ->
            ( { model | message = message }, Cmd.none )

        ToggleApproved approved ->
            ( { model | approved = approved }, Cmd.none )

        Submitted ->
            let
                validated =
                    validateForm model
            in
            -- We save the validation result, so we can display errors in the view function
            ( { model | validated = validated }
            , case validated of
                Err _ ->
                    Cmd.none

                Ok validForm ->
                    postForm validForm
            )


postForm : ValidForm -> Cmd Msg
postForm validForm =
    -- postForm only accepts validated forms
    Debug.log "form submitted" validForm
        |> (\_ -> Cmd.none)


view : Model -> Html Msg
view model =
    form [ onSubmit Submitted ]
        [ div []
            [ label []
                [ div [] [ text "Name" ]
                , input [ onInput InputName, value model.name ] []
                , viewErrors "name" model.validated
                ]
            ]
        , div []
            [ label [ style "width" "200px" ]
                [ div [] [ text "Email" ]
                , input [ onInput InputEmail, value model.email ] []
                , viewErrors "email" model.validated
                ]
            ]
        , div []
            [ label [ style "width" "200px" ]
                [ div [] [ text "Age" ]
                , input [ onInput InputAge, value model.age ] []
                , viewErrors "age" model.validated
                ]
            ]
        , div []
            [ label [ style "width" "200px" ]
                [ div [] [ text "Password" ]
                , input [ onInput InputPassword, value model.password ] []
                , viewErrors "password" model.validated
                ]
            ]
        , div []
            [ label [ style "width" "200px" ]
                [ div [] [ text "Message" ]
                , input [ onInput InputMessage, value model.message ] []
                , viewErrors "message" model.validated
                ]
            ]
        , div []
            [ label [ style "width" "200px" ]
                [ div [] [ text "Approve stuff" ]
                , input [ type_ "checkbox", onCheck ToggleApproved, checked model.approved ] []
                , viewErrors "approved" model.validated
                ]
            ]
        , button [] [ text "Submit" ]
        ]


viewErrors : String -> Validated String ValidForm -> Html Msg
viewErrors fieldName validated =
    case Validator.Named.getErrors fieldName validated of
        Nothing ->
            text ""

        Just errors ->
            div []
                (List.map
                    (\error ->
                        div [ style "color" "red" ] [ text error ]
                    )
                    errors
                )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
