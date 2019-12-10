port module Main exposing (Model, Msg(..), toJs, init, main, update, view, signedIn, signIn)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Browser.Navigation as Nav
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
    , key: Nav.Key
    , url: Url.Url
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


init : Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { counter = flags, serverMessage = "" , idToken = "", events = [], currentEvent = Nothing, key = key, url = url }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
                Sub.batch [ signedIn SignedIn ]


baseURL = "http://192.168.1.21:8081"

type Route
  = Top
  | Signin 
  | EventsList
  | EventDetail String

route : URLP.Parser (Route -> a) a
route =
  URLP.oneOf
    [ URLP.map Top (URLP.s "")
    , URLP.map Signin (URLP.s "")
    , URLP.map EventsList (URLP.s "events")
    , URLP.map EventDetail (URLP.s "event" </> URLP.string)
    ]
-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = SignIn
    | SignedIn String
    | Reload Page
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotEvents (Result Http.Error (List Event))
    | GotEvent (Result Http.Error Event)

type Page = Login
          | Events
          | EventEdit

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SignedIn token ->
            ({ model | idToken = token}, Nav.pushUrl model.key "/events")
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
        GotEvent res ->
            case res of
                Ok r ->
                    ( { model | currentEvent = Just r }, Cmd.none )
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )
        UrlChanged url -> 
            case (URLP.parse route url) of 
                Just EventsList -> ( { model | url = url }, loadEvents model.idToken)
                Just (EventDetail name) -> ( {model | url = url}, loadEvent model.idToken name)
                _ -> ( { model | url = url }, Cmd.none )
        LinkClicked urlRequest -> 
            case urlRequest of
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href -> ( model, Nav.load href )

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

loadEvent : String -> String -> Cmd Msg
loadEvent idToken eventId = Http.request
    { method = "GET"
    , headers = [Http.header "Authorization" ("Bearer " ++ idToken)]
    , body = Http.emptyBody
    , url = baseURL ++ "/api/event/" ++ eventId
    , expect = Http.expectJson GotEvent eventDecoder
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




-- ---------------------------
-- VIEW
-- ---------------------------

eventListView : Model -> Html Msg
eventListView model = case model.events of
    [] -> div [class "container"] [text "empty"
                        , button
                        [ class "pure-button pure-button-primary"
                        ]
                        [ text "Reload" ]
                        ]
    _ -> model.events |> List.map (\ev -> div [ class "container" ] [a [href <| URLB.relative ["event", ev.name] []] [text ev.name], div [] [(eventView ev.slides)]]) |> div []

eventDetailView : Model -> Html Msg 
eventDetailView model = case model.currentEvent of 
    Just event -> div [ class "container" ] [text event.name, div [] [(eventView event.slides)]]
    Nothing -> text "empty"

eventView : List Slide -> Html Msg
eventView slides = slides |> List.map (\s -> ul [] [text s.sdid] ) |> div []

view : Model -> Html Msg
view model = case (model.idToken, URLP.parse route model.url) of
    (_, Just EventsList) -> eventListView model
    (_, Just (EventDetail eventName)) -> eventDetailView model
    (_, _) ->
        div [ class "container" ]
            [ header []
                [ -- img [ src "/images/logo.png" ] []
                span [ class "logo" ] []
                , h1 [] [ text "Elm 0.19.1 Webpack Starter, with hot-reloading" ]
                ]
            , div [ class "pure-g" ]
                [ div [ class "pure-u-1-3" ]
                    [ button
                        [ class "pure-button pure-button-primary"
                        , onClick SignIn
                        ]
                        [ text "Sign in" ]
                    , text <| model.idToken
                    ]
                ]
            ]

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

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest = LinkClicked urlRequest

onUrlChange : Url.Url -> Msg
onUrlChange url = UrlChanged url

main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest 
        , onUrlChange = onUrlChange
        }
