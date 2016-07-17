module Facebook
    exposing
        ( initialModel
        , eventToAction
        , update
        , Event
        , Msg(..)
        , Model
        )

{-| Facebook API for ELM

See the example login application for
a complete usage reference.

# Facebook Javascript SDK

Before requesting any API operation
you must load the SDK and send an
`Init` signal with a `Json.Decode.Value`
of initialization options.

On your application `Main.elm`
you must create as signal for handling
facebook events (eg, SDK has loaded, status
and other FB.Events)

```elm
port facebookEvents: Signal Facebook.Event
```

Then when on your html setup:

```javascript
var app = Elm.fullscreen(Elm.main, {
  facebookEvents: ['NoOp', {}]
});

// Global function called when SDK is loaded
window.fbAsyncInit = function () {

  // Subscribe to statusChange event
  FB.Event.subscribe('auth.statusChange', function (response) {
    app.ports.facebookEvents.send(['StatusChanged', response])
  })

  // Signal the Init event to initialize the SDK
  app.ports.facebookEvents.send(['Init', {
    appId : 'YOUR-APP-ID-XXXX',
    xfbml      : true,
    version    : 'v2.5',
    status     : true,
  }])
}

// The code provided by FB to load SDK
(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
       if (d.getElementById(id)) {return;}
       js = d.createElement(s); js.id = id;
       js.src = "//connect.facebook.net/en_US/sdk.js";
       fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));
```

@docs Event, eventToAction

# Facebook Model

@docs initialModel

# Actions

@docs update, Msg

# Model

@docs Model

-}

import Task exposing (Task)
import Native.Facebook
import Json.Encode
import Json.Decode exposing (decodeValue)


{-| Facebook Actions

Did Action &mdash;
DidValue Action Value &mdash;

-}
type Msg
    = NoOp
    | GET String
    | POST String JsonValue
    | DELETE String
    | Login
    | Logout
    | FbEvent IncomingEvent
    | Did Msg
    | DidValue Msg JsonValue


type IncomingEvent
    = Init InitOptions
    | LoggedIn JsonValue
    | LoggedOut JsonValue
    | AuthResponseChanged JsonValue
    | StatusChanged JsonValue


{-| Facebook Model
-}
type alias Model =
    { ready : Bool, id : Maybe String }


type alias EventName =
    String


type alias JsonValue =
    Json.Decode.Value


{-| Type of events sent to your application port
-}
type alias Event =
    ( EventName, JsonValue )


type alias InitOptions =
    JsonValue


{-| Convert an Event tuple into an actual Action
-}
eventToAction : ( EventName, JsonValue ) -> Msg
eventToAction ( eventName, jsonValue ) =
    case eventName of
        "Init" ->
            FbEvent (Init jsonValue)

        "StatusChanged" ->
            FbEvent (StatusChanged jsonValue)

        "AuthResponseChanged" ->
            FbEvent (AuthResponseChanged jsonValue)

        _ ->
            NoOp


{-| The initial model

Initially the API is not ready until
an Init signal is seen.
-}
initialModel : Model
initialModel =
    { ready = False, id = Nothing }


updateOnInit : InitOptions -> Msg -> Model -> ( Model, Task String Msg )
updateOnInit options action model =
    init options |> always (noEffects { model | ready = True })


authResponseUserId : Json.Decode.Decoder String
authResponseUserId =
    Json.Decode.at [ "authResponse", "userID" ] Json.Decode.string


updateOnStatusChanged : JsonValue -> Msg -> Model -> ( Model, Task String Msg )
updateOnStatusChanged jsonValue action model =
    let
        decoded =
            decodeValue authResponseUserId jsonValue
    in
        case decoded of
            Ok id ->
                connectedEffects id model

            _ ->
                disconnectedEffects model


{-| Step model and produce possible effects
-}
update : Msg -> Model -> ( Model, Task String Msg )
update action model =
    case action of
        Login ->
            login () |> always (noEffects model)

        Logout ->
            logout () |> always (noEffects model)

        FbEvent (Init options) ->
            updateOnInit options action model

        FbEvent (StatusChanged jsonValue) ->
            updateOnStatusChanged jsonValue action model

        GET path ->
            apiRequestEffects "GET" path emptyJsonValue action model

        POST path jsonValue ->
            apiRequestEffects "POST" path jsonValue action model

        DELETE path ->
            apiRequestEffects "DELETE" path emptyJsonValue action model

        _ ->
            noEffects model


noEffects : Model -> ( Model, Task String Msg )
noEffects model =
    ( model, Task.fail "noEffects" )


connectedEffects : String -> Model -> ( Model, Task String Msg )
connectedEffects facebookId model =
    let
        new_model =
            { model | id = Just facebookId }

        effects =
            Task.succeed (Did Login)
    in
        ( new_model, effects )


disconnectedEffects : Model -> ( Model, Task String Msg )
disconnectedEffects model =
    let
        new_model =
            { model | id = Nothing }

        effects =
            Task.succeed (Did Logout)
    in
        ( new_model, effects )


emptyJsonValue : Json.Encode.Value
emptyJsonValue =
    Json.Encode.object []


apiRequestEffects : String -> String -> JsonValue -> Msg -> Model -> ( Model, Task String Msg )
apiRequestEffects method path data action model =
    let
        requestEffects =
            apiTask ( method, path, data )

        didEffects =
            Task.map (DidValue action) requestEffects
    in
        ( model, didEffects )


apiTask : ( String, String, JsonValue ) -> Task.Task x JsonValue
apiTask =
    Native.Facebook.api


init : InitOptions -> ()
init =
    Native.Facebook.init


login : () -> ()
login =
    Native.Facebook.login


logout : () -> ()
logout =
    Native.Facebook.logout
