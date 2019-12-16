module Pages.EventCreate exposing(..)
import Types exposing(..)
import Config exposing (..)
import Util.Http exposing(..)
import Util.Event exposing(..)
import Session
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

type alias Model = { event: Event
                   , session: Session.Data
                   }

type Msg = NoOp
    | EventModified ModifyAction
    | CreateEvent Event
    | SavedEvent (Result Http.Error Event)

type ModifyAction = Swap Int Int
                  | ChangeSlide Int Slide
                  | AddSlide 
                  | DeleteSlide Int
                  | NameChanged String

init : Session.Data -> (Model, Cmd Msg)
init session = case session.events of
    Just events -> 
        (Model (Event "" [] "" "") session, Cmd.none)
    Nothing -> 
        (Model (Event "" [] "" "") session, Cmd.none)
update : Msg -> Model -> ( Model, Cmd Msg )
update message model = case message of
        NoOp -> (model, Cmd.none)
        EventModified modifyAction ->
            case modifyAction of
                        Swap a b -> 
                            let
                                oldCurrentEvent = model.event
                                newCurrentEvent =  ( { oldCurrentEvent | slides = swapAt a b model.event.slides })
                            in ({model | event = newCurrentEvent}, Cmd.none)
                        ChangeSlide idx slide -> 
                            let
                                oldCurrentEvent = model.event
                                newCurrentEvent =  { oldCurrentEvent | slides = setAt idx slide model.event.slides }
                            in ({model | event = newCurrentEvent}, Cmd.none)
                        AddSlide -> 
                            let
                                oldCurrentEvent = model.event
                                newCurrentEvent =  ( { oldCurrentEvent | slides = Slide "" 0 :: model.event.slides })
                            in ({model | event= newCurrentEvent}, Cmd.none)
                        DeleteSlide idx -> 
                            let
                                deleteItem i list = List.take i list ++ List.drop (i + 1) list
                                oldCurrentEvent = model.event
                                newCurrentEvent =  ( { oldCurrentEvent | slides = deleteItem idx model.event.slides })
                            in ({model | event = newCurrentEvent}, Cmd.none)
                        NameChanged name ->
                            let
                                oldCurrentEvent = model.event
                                newCurrentEvent =  ( { oldCurrentEvent | name = name})
                            in ({model | event = newCurrentEvent}, Cmd.none)
        CreateEvent event -> 
                    (model, createEvent model.session.idToken event)
        SavedEvent res ->
            case res of
                Ok r ->
                        ( model, Nav.pushUrl model.session.key (URLB.absolute ["events"] []))
                Err err ->
                    ( model, handleHttpError model.session.key err)



view : Model -> Html Msg
view model = div [] [input [onInput (\newName -> EventModified (NameChanged newName)), value model.event.name] []
                                    , button [ class "pure-button pure-button-primary"
                                    , onClick <| EventModified AddSlide ] [text "Add"]
                                    , button [ class "pure-button pure-button-primary", onClick <| CreateEvent model.event] [text "Save"]
                                    , model.event.slides |> List.indexedMap slideEditRow |> div []
                                        ]

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