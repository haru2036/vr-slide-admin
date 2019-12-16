module Pages.EventEdit exposing(..)
import Config exposing (..)
import Types exposing(..)
import Session
import Util.Event exposing(..)
import Util.Http exposing(..)
import Encoder exposing(..)
import Decoder exposing(..)
import List.Extra exposing (setAt, swapAt)
import Http exposing (Error(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Browser.Navigation as Nav
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row 
import Bootstrap.Form.Input as Input
import String exposing(toInt, fromInt)

type alias Model = { event: Maybe Event
                   , session: Session.Data
                   }

type Msg = NoOp
    | GotEvent (Result Http.Error Event)
    | EventModified ModifyAction
    | SaveEvent Event
    | SavedEvent (Result Http.Error Event)
    | DeleteEvent Event
    | DeletedEvent (Result Http.Error ())

init : String -> Session.Data -> (Model, Cmd Msg)
init eventId session = case session.events of
    Just events -> 
        (Model Nothing session, loadEvent session.idToken eventId)
    Nothing -> 
        (Model Nothing session, loadEvent session.idToken eventId)

type ModifyAction = Swap Int Int
                  | ChangeSlide Int Slide
                  | AddSlide 
                  | DeleteSlide Int
                  | NameChanged String

update : Msg -> Model -> ( Model, Cmd Msg )
update message model = case message of
        NoOp ->
            (model, Cmd.none)
        GotEvent res ->
            case res of
                Ok r ->
                    ({ model | event = Just r} , Cmd.none )
                Err err ->
                    ( { model | event = Nothing }, Cmd.none)
        EventModified modifyAction ->
            case model.event of
                Just event ->
                    case modifyAction of
                        Swap a b -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = swapAt a b event.slides })
                            in ({ model | event = newCurrentEvent}, Cmd.none)
                        ChangeSlide idx slide -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just { oldCurrentEvent | slides = setAt idx slide event.slides }
                            in ({ model | event = newCurrentEvent}, Cmd.none)
                        AddSlide -> 
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = Slide "" 0 :: event.slides })
                            in ({ model | event = newCurrentEvent}, Cmd.none)
                        DeleteSlide idx -> 
                            let
                                deleteItem i list = List.take i list ++ List.drop (i + 1) list
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | slides = deleteItem idx event.slides })
                            in ({ model | event = newCurrentEvent}, Cmd.none)
                        NameChanged name ->
                            let
                                oldCurrentEvent = event
                                newCurrentEvent = Just ( { oldCurrentEvent | name = name})
                            in ({ model | event = newCurrentEvent}, Cmd.none)
                Nothing -> ( model, Cmd.none ) 
        SaveEvent event -> 
                    (model, saveEvent model.session.idToken event)
        DeleteEvent event -> 
                    (model, deleteEvent model.session.idToken event)
        SavedEvent res ->
            case res of
                Ok r ->
                    case model.event of
                        Just ev -> 
                            ( model, Nav.pushUrl model.session.key (URLB.absolute ["events"] []))
                        Nothing -> (model, Cmd.none)
                Err err ->
                    ( { model | event = Nothing }, Cmd.none)
        DeletedEvent res ->
            case res of
                Ok r ->
                    ( { model | event = Nothing }, Nav.pushUrl model.session.key (URLB.absolute ["events"] [] ))
                Err err ->
                    ( { model | event = Nothing }, Cmd.none)

view : Model -> Html Msg
view model = case model.event of
                Just event -> div [] [input [onInput (\newName -> EventModified (NameChanged newName)), value event.name] []
                                    , button [ class "pure-button pure-button-primary", onClick <| EventModified AddSlide ] [text "Add"]
                                    , button [ class "pure-button pure-button-primary", onClick <| SaveEvent event] [text "Save"]
                                    , Button.button [ Button.danger, Button.attrs [onClick <| DeleteEvent event]] [text "Delete"]
                                    , event.slides |> List.indexedMap slideEditRow |> div []
                                        ]
                Nothing -> button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]

slideEditRow : Int -> Slide -> Html Msg
slideEditRow index slide = Grid.container [ class "card"
                                          , style "background-image" ("url(\"" ++ firstPageUrl slide.sdid ++ "\")")
                                          , style "background-size" "contain"
                                          , style "background-repeat" "no-repeat"
                                          ] 
                               [ Grid.row [ Row.attrs <| [class "slide-edit-content"]] [
                                   Grid.col [] [
                                    div [] 
                                        [ text "SlideID: "
                                        , Input.text [Input.attrs [onInput (\newSdid -> EventModified (ChangeSlide index {slide | sdid = newSdid })) , value slide.sdid]]  
                                        ]
                                    , div [] 
                                        [ text "Slide Count"
                                        , Input.number [Input.attrs [onInput (\newCount -> case toInt newCount of
                                                                                    Just count -> EventModified (ChangeSlide index {slide | count = count })
                                                                                    Nothing -> NoOp
                                                                        ) , value <| fromInt slide.count]]
                                        ]
                                   ]
                               , Grid.col [] [
                                    button [ class "pure-button"
                                        , onClick <| EventModified (Swap index (index - 1)) ]
                                        [ text "Up" ]
                                    , button [ class "pure-button"
                                        , onClick <| EventModified (Swap index (index + 1)) ]
                                        [ text "Down" ]
                                    , Button.button [Button.warning, Button.attrs [onClick <| EventModified (DeleteSlide index )] ]
                                        [ text "Delete" ]
                                ] 
                               ] 
                               ]



handleHttpError : Nav.Key -> Http.Error -> Cmd Msg
handleHttpError key err = 
    case err of
        BadStatus 401 ->
            Nav.pushUrl key <| URLB.absolute ["registration", "information"] []
        _ -> Cmd.none

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