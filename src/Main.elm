port module Main exposing (Model, Msg(..), toJs, init, main, update, view, signedIn, signIn)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (setAt, swapAt)
import String exposing(fromInt, toInt)
import Url
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Browser.Navigation as Nav
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode



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
    { eventId: String
    , slides: List Slide
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
  | EventModify String
  | EventCreate 

route : URLP.Parser (Route -> a) a
route =
  URLP.oneOf
    [ URLP.map Signin (URLP.s "")
    , URLP.map EventsList (URLP.s "events")
    , URLP.map EventCreate (URLP.s "event" </> URLP.s "new")
    , URLP.map EventDetail (URLP.s "event" </> URLP.string)
    , URLP.map EventModify (URLP.s "event" </> URLP.s "edit" </> URLP.string)
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
    | EventModified ModifyAction
    | SaveEvent Event
    | CreateEvent Event
    | SavedEvent (Result Http.Error Event)
    | NoOp

type ModifyAction = Swap Int Int
                  | ChangeSlide Int Slide
                  | AddSlide 
                  | DeleteSlide Int
                  | NameChanged String

type Page = Login
          | Events
          | EventEdit

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
                Just (EventDetail uuid) -> ( {model | url = url}, loadEvent model.idToken uuid)
                Just (EventModify uuid) -> ( {model | url = url}, loadEvent model.idToken uuid)
                Just EventCreate -> ( {model | url = url, currentEvent = Just (Event "" [] "" "")}, Cmd.none)
                _ -> ( { model | url = url }, Cmd.none )
        LinkClicked urlRequest -> 
            case urlRequest of
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href -> ( model, Nav.load href )
        SaveEvent event -> 
                    (model, saveEvent model.idToken event)
        CreateEvent event -> 
                    (model, createEvent model.idToken event)
        SavedEvent res ->
            case res of
                Ok r ->
                    case model.currentEvent of
                        Just ev -> 
                            ( model, Nav.pushUrl model.key (URLB.absolute ["event", ev.eventId] []))
                        Nothing -> (model, Cmd.none)
                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )
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




-- ---------------------------
-- VIEW
-- ---------------------------

eventEditView : Model -> Html Msg
eventEditView model = case model.currentEvent of
                Just event -> div [] [input [onInput (\newName -> EventModified (NameChanged newName)), value event.name] []
                                    , button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]
                                    , button [ class "pure-button pure-button-primary", onClick <| SaveEvent event] [text "Save"]
                                    , event.slides |> List.indexedMap slideEditRow |> div []
                                        ]
                Nothing -> button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]

eventCreateView : Model -> Html Msg
eventCreateView model = case model.currentEvent of
                Just event -> div [] [input [onInput (\newName -> EventModified (NameChanged newName)), value event.name] []
                                    , button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]
                                    , button [ class "pure-button pure-button-primary", onClick <| CreateEvent event] [text "Save"]
                                    , event.slides |> List.indexedMap slideEditRow |> div []
                                        ]
                Nothing -> button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]

slideEditRow : Int -> Slide -> Html Msg
slideEditRow index slide = div [ class "card"] 
                               [ div [] 
                                    [ text "SlideID: "
                                    , input [onInput (\newSdid -> EventModified (ChangeSlide index {slide | sdid = newSdid })) , value slide.sdid ] []
                                    ]
                               , div [] 
                                    [ text "Slide Count"
                                    , input [type_ "number", onInput (\newCount -> case toInt newCount of
                                                                                Just count -> EventModified (ChangeSlide index {slide | count = count })
                                                                                Nothing -> NoOp
                                                                    ) , value <| fromInt slide.count] [] 
                                    ]
                               , button [ class "pure-button button-warning"
                                        , onClick <| EventModified (DeleteSlide index ) ]
                                        [ text "Delete" ]
                               , button [ class "pure-button"
                                        , onClick <| EventModified (Swap index (index - 1)) ]
                                        [ text "Up" ]
                               , button [ class "pure-button"
                                        , onClick <| EventModified (Swap index (index + 1)) ]
                                        [ text "Down" ]
                               ] 

eventListView : Model -> Html Msg
eventListView model = case model.events of
    [] -> div [class "container"] [text "empty"
                        , button
                        [ class "pure-button pure-button-primary"
                        ]
                        [ text "Reload" ]
                        ]
    _ -> div [class "container" ] [ a
                                    [ class "pure-button pure-button-primary"
                                    , href <| URLB.absolute["event", "new"] []] [ text "Create new"]
                                  , model.events |> List.map (\ev -> div [ class "card" ] [a [href <| URLB.absolute ["event", ev.eventId] []] [text ev.name], div [] [(slideListView ev.slides)]]) |> div []]

eventDetailView : Model -> Html Msg 
eventDetailView model = case model.currentEvent of 
    Just event -> div [ class "container" ] [text event.name, a [href <| URLB.absolute ["event", "edit", event.eventId] []] [text "edit"], div [] [(slideListView event.slides)]]
    Nothing -> text "empty"

slideListView : List Slide -> Html Msg
slideListView slides = slides |> List.map (\s -> ul [] [text s.sdid] ) |> div []

view : Model -> Html Msg
view model = case (model.idToken, URLP.parse route model.url) of
    (_, Just EventsList) -> eventListView model
    (_, Just (EventDetail eventName)) -> eventDetailView model
    (_, Just (EventModify eventName)) -> eventEditView model
    (_, Just EventCreate) -> eventCreateView model
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

-- ---------------------------
-- ENCODER
-- ---------------------------

eventsEncoder : List Event -> Encode.Value
eventsEncoder events = Encode.list eventEncoder events

eventEncoder : Event -> Encode.Value
eventEncoder event = Encode.object
                        [ ("name", Encode.string event.name)
                        , ("slides", Encode.list slideEncoder event.slides)]
                        
slideEncoder : Slide -> Encode.Value
slideEncoder slide = Encode.object
                    [ ("sdid", Encode.string slide.sdid) 
                    , ("count", Encode.int slide.count)]

-- ---------------------------
-- DECODER
-- ---------------------------

eventsDecoder: Decode.Decoder (List Event)
eventsDecoder = Decode.list eventDecoder

eventDecoder : Decode.Decoder Event
eventDecoder =
  Decode.map4 Event
    (Decode.field "uuid" Decode.string)
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
