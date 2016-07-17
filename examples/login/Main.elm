port module Main exposing (..)

import Html exposing (div, text, button, img, small, a)
import Html.Attributes exposing (src, href)
import Html.Events exposing (onClick)
import Html.App as App
import Json.Decode
import Task
import Platform.Cmd
import Facebook


-- login.html sends values to this signal


port facebookEvents : (( String, Json.Decode.Value ) -> a) -> Sub a


type Msg
    = NoOp
    | FacebookRequest Facebook.Msg
    | FacebookResponse Facebook.Msg
    | FacebookError String


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- initial actions at application startup
-- we listen for when FB api has loaded


subscriptions : a -> Sub Msg
subscriptions model =
    facebookEvents (Facebook.eventToAction >> FacebookRequest)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { facebook = Facebook.initialModel
    , name = Nothing
    }


type alias Model =
    { facebook : Facebook.Model, name : Maybe String }



-- Just perform Facebook.update and tag resulting effects


updateOnFacebookRequest : (Facebook.Msg -> Msg) -> Facebook.Msg -> Model -> ( Model, Cmd Msg )
updateOnFacebookRequest tag request model =
    let
        ( fb_model, fb_effects ) =
            Facebook.update request model.facebook

        new_model =
            { model | facebook = fb_model }
    in
        ( new_model, Task.perform FacebookError FacebookResponse fb_effects )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    -- Uncomment for debugging all reactions
    -- let _ = Debug.log "update" (action, model) in
    case action of
        -- Transform fb events into effects
        FacebookRequest request ->
            updateOnFacebookRequest FacebookResponse request model

        -- Reset name when user has logged out
        FacebookResponse (Facebook.Did (Facebook.Logout)) ->
            ( { model | name = Nothing }, Cmd.none )

        -- When user logins request her info via graph api
        FacebookResponse (Facebook.Did (Facebook.Login)) ->
            let
                ( requestUserInfoModel, requestUserInfoMessage ) =
                    Facebook.update (Facebook.GET "me") model.facebook
            in
                ( model
                , Task.perform FacebookError FacebookResponse requestUserInfoMessage
                )

        -- When got me, extract the user name from response
        FacebookResponse (Facebook.DidValue (Facebook.GET "me") jsonValue) ->
            let
                decoder =
                    Json.Decode.at [ "name" ] Json.Decode.string

                decoded =
                    Json.Decode.decodeValue decoder jsonValue

                new_model =
                    case decoded of
                        Err _ ->
                            { model | name = Nothing }

                        Ok name ->
                            { model | name = Just name }
            in
                ( new_model, Cmd.none )

        -- Handle all your app actions here
        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ case model.facebook.ready of
            False ->
                text "Loading Facebook API"

            True ->
                facebookLoginView model
        , div []
            [ a [ href "http://github.com/vic/elm-facebook" ] [ text "github.com/vic/elm-facebook" ]
            ]
        ]


username : Maybe String -> String
username name =
    case name of
        Nothing ->
            ""

        Just x ->
            x


facebookLoginView : Model -> Html.Html Msg
facebookLoginView { facebook, name } =
    div []
        [ div [] [ text "ELM Facebook Login Example" ]
        , case facebook.id of
            Nothing ->
                button [ onClick (FacebookRequest Facebook.Login) ]
                    [ text "Login with Facebook" ]

            Just id ->
                div []
                    [ div [] [ img [ src ("http://graph.facebook.com/" ++ id ++ "/picture?type=large") ] [] ]
                    , div [] [ text <| "Welcome " ++ (username name) ]
                    , button [ onClick (FacebookRequest Facebook.Logout) ]
                        [ text "Logout" ]
                    ]
        ]
