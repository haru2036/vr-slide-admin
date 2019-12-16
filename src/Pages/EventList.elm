module Pages.EventList exposing(..)
import Types exposing(..)
import List
import Session
import Types exposing(..)
import Config exposing (..)
import Util.Http exposing(..)
import Util.Event exposing(..)
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


type alias Model = { events: List Event
                   , session: Session.Data
                   }

type Msg = GotEvents (Result Http.Error (List Event))

init : Session.Data -> (Model, Cmd Msg)
init session = case session.events of
    Just events -> 
        (Model events session, Cmd.none)
    Nothing -> 
        (Model [] session, loadEvents session.idToken)

update : Msg -> Model -> ( Model, Cmd Msg )
update message model = case message of
        GotEvents res ->
            case res of
                Ok r ->
                    ({ model | events = r}, Cmd.none )
                Err err ->
                    ({ model | events = []}, handleHttpError model.session.key err )

view : Model -> Html Msg
view model = case model.events of
    [] -> div [class "container"] [text "empty"
                        , a [ class "pure-button pure-button-primary", href <| URLB.absolute["event", "new"] []] [ text "Create new"]
                        ]
    _ -> div [class "container" ] [ a
                                    [ class "pure-button pure-button-primary"
                                    , href <| URLB.absolute["event", "new"] []] [ text "Create new"]
                                  , model.events |> List.map (\ev -> div [ class "card" ] [a [href <| URLB.absolute ["event", "edit", ev.eventId] []] [text ev.name], div [] [(slideListView ev.slides)]]) |> div []]

slideListView : List Slide -> Html Msg
slideListView slides = slides |> List.map (\s -> ul [] [text s.sdid] ) |> div []

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

handleHttpError : Nav.Key -> Http.Error -> Cmd Msg
handleHttpError key err = 
    case err of
        BadStatus 401 ->
            Nav.pushUrl key <| URLB.absolute ["registration", "information"] []
        _ -> Cmd.none