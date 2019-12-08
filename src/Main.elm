port module Main exposing (Model, Msg(..), add1, init, main, toJs, update, view, signedIn)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg

port signIn : () -> Cmd msg

port signedIn : (String -> msg) -> Sub msg


-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { counter : Int
    , serverMessage : String
    , idToken : String
    , events: List Event
    , currentEvent: Maybe Event
    }

type alias Event = 
    { slides: List Slide
    , authorId: String
    , name: String
    }

type alias Slide = 
    { sdid: String
    , count: Int
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { counter = flags, serverMessage = "" , idToken = "", events = [], currentEvent = Nothing }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
                Sub.batch [ signedIn SignedIn ]


baseURL = "http://192.168.1.21:8081"
-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Inc
    | Set Int
    | TestServer
    | OnServerResponse (Result Http.Error String)
    | SignIn
    | SignedIn String
    | Reload Page
    | GotEvents (Result Http.Error (List Event))

type Page = Login
          | Events
          | EventEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, toJs "Hello Js" )

        Set m ->
            ( { model | counter = m }, toJs "Hello Js" )

        TestServer ->
            let
                expect =
                    Http.expectJson OnServerResponse (Decode.field "result" Decode.string)
            in
            ( model
            , Http.get { url = "/test", expect = expect }
            )

        OnServerResponse res ->
            case res of
                Ok r ->
                    ( { model | serverMessage = r }, Cmd.none )

                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )
        SignedIn token ->
            ({ model | idToken = token}, Cmd.none )
        SignIn -> (model, signIn ())
        Reload page ->
            case page of
                Events -> (model, loadEvents model.idToken)
                _ -> (model, Cmd.none)
        GotEvents res ->
            case res of
                Ok r ->
                    ( { model | events = r }, Cmd.none )
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )

loadEvents : String -> Cmd Msg
loadEvents idToken = Http.request
    { method = "GET"
    , headers = [Http.header "Authorization" ("Bearer " ++ idToken)]
    , body = Http.emptyBody
    , url = baseURL ++ "/api/events"
    , expect = Http.expectJson GotEvents eventsDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl url ->
            "BadUrl: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s


{-| increments the counter

    add1 5 --> 6

-}
add1 : Model -> Model
add1 model =
    { model | counter = model.counter + 1 }



-- ---------------------------
-- VIEW
-- ---------------------------

eventListView : Model -> Html Msg
eventListView model = case model.events of
    [] -> div [class "container"] [text "empty"
                        , button
                        [ class "pure-button pure-button-primary"
                        , onClick <| Reload Events
                        ]
                        [ text "Reload" ]
                        ,
                        button
                        [ class "pure-button pure-button-primary"
                        , onClick SignIn
                        ]
                        [ text "Sign in" ]
                        ]
    _ -> model.events |> List.map (\ev -> div [ class "container" ] [text ev.name, div [] [(eventView ev.slides)]]) |> div []

eventView : List Slide -> Html Msg
eventView slides = slides |> List.map (\s -> ul [] [text s.sdid] ) |> div []

view : Model -> Html Msg
view model = case (model.idToken, model.events, model.currentEvent) of
    ("", _, _) ->
        div [ class "container" ]
            [ header []
                [ -- img [ src "/images/logo.png" ] []
                span [ class "logo" ] []
                , h1 [] [ text "Elm 0.19.1 Webpack Starter, with hot-reloading" ]
                ]
            , p [] [ text "Click on the button below to increment the state." ]
            , div [ class "pure-g" ]
                [ div [ class "pure-u-1-3" ]
                    [ button
                        [ class "pure-button pure-button-primary"
                        , onClick Inc
                        ]
                        [ text "+ 1" ]
                    , text <| String.fromInt model.counter
                    ]
                , div [ class "pure-u-1-3" ] 
                    [ button
                        [ class "pure-button pure-button-primary"
                        , onClick SignIn
                        ]
                        [ text "Sign in" ]
                    , text <| model.idToken
                    ]
                , div [ class "pure-u-1-3" ]
                    [ button
                        [ class "pure-button pure-button-primary"
                        , onClick TestServer
                        ]
                        [ text "ping dev server" ]
                    , text model.serverMessage
                    ]
                ]
            , p [] [ text "Then make a change to the source code and see how the state is retained after you recompile." ]
            , p []
                [ text "And now don't forget to add a star to the Github repo "
                , a [ href "https://github.com/simonh1000/elm-webpack-starter" ] [ text "elm-webpack-starter" ]
                ]
            ]
    (_, _, _) -> eventListView model

-- DECODER

eventsDecoder: Decode.Decoder (List Event)
eventsDecoder = Decode.list eventDecoder

eventDecoder : Decode.Decoder Event
eventDecoder =
  Decode.map3 Event
    (Decode.field "slides" (Decode.list slideDecoder))
    (Decode.field "authorId" Decode.string)
    (Decode.field "name" Decode.string)

slideDecoder : Decode.Decoder Slide
slideDecoder = 
    Decode.map2 Slide
        (Decode.field "sdid" Decode.string)
        (Decode.field "count" Decode.int)

-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }
