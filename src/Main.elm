port module Main exposing ( toJs, init, main, update, view, signedIn, signIn)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (setAt, swapAt)
import Url
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Browser.Navigation as Nav
import Http exposing (Error(..))
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Model exposing (..)
import Msg exposing (..)
import View.Event exposing (..)
import Encoder exposing(..)
import Decoder exposing(..)




-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg

port signIn : () -> Cmd msg

port signedIn : (String -> msg) -> Sub msg



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
  | EventModify String
  | EventCreate 
  | RegisterRequired

route : URLP.Parser (Route -> a) a
route =
  URLP.oneOf
    [ URLP.map Signin (URLP.s "")
    , URLP.map EventsList (URLP.s "events")
    , URLP.map EventCreate (URLP.s "event" </> URLP.s "new")
    , URLP.map EventModify (URLP.s "event" </> URLP.s "edit" </> URLP.string)
    , URLP.map RegisterRequired (URLP.s "registration" </> URLP.s "information")
    ]
-- ---------------------------
-- UPDATE
-- ---------------------------



update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp -> (model, Cmd.none)
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
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, handleHttpError model.key err )
        GotEvent res ->
            case res of
                Ok r ->
                    ( { model | currentEvent = Just r }, Cmd.none )
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, handleHttpError model.key err)
        UrlChanged url -> 
            case (URLP.parse route url) of 
                Just EventsList -> ( { model | url = url }, loadEvents model.idToken)
                Just (EventModify uuid) -> ( {model | url = url}, loadEvent model.idToken uuid)
                Just EventCreate -> ( {model | url = url, currentEvent = Just (Event "" [] "" "")}, Cmd.none)
                _ -> ( { model | url = url }, Cmd.none )
        LinkClicked urlRequest -> 
            case urlRequest of
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href -> ( model, Nav.load href )
        SaveEvent event -> 
                    (model, saveEvent model.idToken event)
        DeleteEvent event -> 
                    (model, deleteEvent model.idToken event)
        CreateEvent event -> 
                    (model, createEvent model.idToken event)
        SavedEvent res ->
            case res of
                Ok r ->
                    case model.currentEvent of
                        Just ev -> 
                            ( model, Nav.pushUrl model.key (URLB.absolute ["events"] []))
                        Nothing -> (model, Cmd.none)
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, handleHttpError model.key err)
        DeletedEvent res ->
            case res of
                Ok r ->
                    ( { model | currentEvent = Nothing }, Nav.pushUrl model.key (URLB.absolute ["events"] [] ))
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, handleHttpError model.key err)
        EventModified modifyAction ->
            case model.currentEvent of
                Just event ->
                    case modifyAction of
                        Swap a b -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = swapAt a b event.slides })
                            in ({model | currentEvent = newCurrentEvent}, Cmd.none)
                        ChangeSlide idx slide -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just { oldCurrentEvent | slides = setAt idx slide event.slides }
                            in ({model | currentEvent = newCurrentEvent}, Cmd.none)
                        AddSlide -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = Slide "" 0 :: event.slides })
                            in ({model | currentEvent = newCurrentEvent}, Cmd.none)
                        DeleteSlide idx -> 
                            let
                                deleteItem i list = List.take i list ++ List.drop (i + 1) list
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = deleteItem idx event.slides })
                            in ({model | currentEvent = newCurrentEvent}, Cmd.none)
                        NameChanged name ->
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | name = name})
                            in ({model | currentEvent = newCurrentEvent}, Cmd.none)
                Nothing -> ( model, Cmd.none ) 


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

saveEvent : String -> Event -> Cmd Msg
saveEvent idToken event = Http.request
    { method = "PUT"
    , headers = [Http.header "Authorization" ("Bearer " ++ idToken)]
    , body = Http.jsonBody <| eventEncoder event
    , url = baseURL ++ "/api/event/" ++ event.eventId
    , expect = Http.expectJson SavedEvent eventDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

deleteEvent : String -> Event -> Cmd Msg
deleteEvent idToken event = Http.request
    { method = "Delete"
    , headers = [Http.header "Authorization" ("Bearer " ++ idToken)]
    , body = Http.jsonBody <| eventEncoder event
    , url = baseURL ++ "/api/event/" ++ event.eventId
    , expect = Http.expectWhatever DeletedEvent 
    , timeout = Nothing
    , tracker = Nothing
    }

createEvent : String -> Event -> Cmd Msg
createEvent idToken event = Http.request
    { method = "PUT"
    , headers = [Http.header "Authorization" ("Bearer " ++ idToken)]
    , body = Http.jsonBody <| eventEncoder event
    , url = baseURL ++ "/api/event/new" 
    , expect = Http.expectJson SavedEvent eventDecoder
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

handleHttpError : Nav.Key -> Http.Error -> Cmd Msg
handleHttpError key err = 
    case err of
        BadStatus 401 ->
            Nav.pushUrl key <| URLB.absolute ["registration", "information"] []
        _ -> Cmd.none




-- ---------------------------
-- VIEW
-- ---------------------------
registerInformationView : Model -> Html Msg
registerInformationView model = 
        div [ class "container" ]
            [ header []
                [ -- img [ src "/images/logo.png" ] []
                  h1 [] [ text "Registration Required" ]
                , text "このサービスは現在運営に許可されたユーザのみが使えるサービスです。もしあなたが許可されたユーザの場合はRegisterボタンを押してその旨を運営にお知らせください。"
                ]
            , div [ class "pure-g" ]
                [ div [ class "pure-u-1-3" ]
                    [   
                        button
                        [ class "pure-button pure-button-primary"
                        , href <| URLB.absolute ["registration", "apply"] []
                        ]
                        [ text "Register" ]
                    ]
                ]
            ]
loginView : Model -> Html Msg
loginView model =
        div [ class "container" ]
            [ header []
                [ -- img [ src "/images/logo.png" ] []
                span [ class "logo" ] []
                , h1 [] [ text "VRC-LT スライド管理" ]
                ]
                , text "このサービスは現在VRC-LT運営に許可されたユーザのみが使えるサービスとなっています。ログインした後に運営から許可されたユーザのみ利用できます。"
            , div [ class "pure-g" ]
                [ div [ class "pure-u-1-3" ]
                    [ button
                        [ class "pure-button pure-button-primary"
                        , onClick SignIn
                        ]
                        [ text "Sign in" ]
                    ]
                ]
            ]

view : Model -> Html Msg
view model = 
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [
                    case (model.idToken, URLP.parse route model.url) of
                        (_, Just EventsList) -> eventListView model
                        (_, Just (EventModify eventName)) -> eventEditView model
                        (_, Just EventCreate) -> eventCreateView model
                        (_, Just RegisterRequired) -> registerInformationView model
                        (_, _) -> loginView model
                ]
            ]

        ]


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
