port module Main exposing ( toJs, init, main, update, view, signedIn, signIn)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Browser.Navigation as Nav
import Http exposing (Error(..))
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Encoder exposing(..)
import Decoder exposing(..)
import Session
import Pages.EventList as EL
import Pages.EventCreate as EC
import Pages.EventEdit as EE
import Types exposing (..)




-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg

port signIn : () -> Cmd msg

port signedIn : (String -> msg) -> Sub msg



init : Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { counter = flags, serverMessage = "" , session = Session.empty key , page = Login, key = key, url = url }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
                Sub.batch [ signedIn SignedIn ]


-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { counter : Int
    , serverMessage : String
    , session: Session.Data
    , page: Page
    , key: Nav.Key
    , url: Url.Url
    }

type Page = Login
        | EventList EL.Model
        | EventCreate EC.Model
        | EventEdit EE.Model
        | RegisterRequired

type Route
  = RTop
  | RSignin 
  | REventsList
  | REventModify String
  | REventCreate 
  | RRegisterRequired

route : URLP.Parser (Route -> a) a
route =
  URLP.oneOf
    [ URLP.map RSignin (URLP.s "")
    , URLP.map REventsList (URLP.s "events")
    , URLP.map REventCreate (URLP.s "event" </> URLP.s "new")
    , URLP.map REventModify (URLP.s "event" </> URLP.s "edit" </> URLP.string)
    , URLP.map RRegisterRequired (URLP.s "registration" </> URLP.s "information")
    ]
-- ---------------------------
-- UPDATE
-- ---------------------------

type Msg
    = SignIn
    | SignedIn String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | NoOp
    | EventListMsg EL.Msg
    | EventCreateMsg EC.Msg
    | EventEditMsg EE.Msg




update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp -> (model, Cmd.none)
        SignedIn token ->
            let oldSession = model.session
                newSession = { oldSession | idToken = token}
            in ({ model | session = newSession }, Nav.pushUrl model.key "/events")
        SignIn -> (model, signIn ())
        UrlChanged url -> stepUrl url model
        LinkClicked urlRequest -> 
            case urlRequest of
                Browser.Internal url -> (model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href -> ( model, Nav.load href )
        EventListMsg msg -> case model.page of
            EventList eventList -> stepEventList model (EL.update msg eventList)
            _ -> (model, Cmd.none)
        EventEditMsg msg -> case model.page of
            EventEdit eventEdit -> stepEventEdit model (EE.update msg eventEdit)
            _ -> (model, Cmd.none)
        EventCreateMsg msg -> case model.page of
            EventCreate eventCreate -> stepEventCreate model (EC.update msg eventCreate)
            _ -> (model, Cmd.none)

stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
            case (URLP.parse route url) of 
                Just REventsList -> stepEventList model (EL.init model.session)
                Just (REventModify uuid) -> stepEventEdit model (EE.init uuid model.session)
                Just REventCreate -> stepEventCreate model (EC.init model.session)
                _ -> ( { model | url = url }, Cmd.none )

stepEventList : Model -> ( EL.Model, Cmd EL.Msg ) -> ( Model, Cmd Msg )
stepEventList model (eventList, cmds) =
  ( { model | page = EventList eventList }
  , Cmd.map EventListMsg cmds
  )

stepEventEdit : Model -> ( EE.Model, Cmd EE.Msg ) -> ( Model, Cmd Msg )
stepEventEdit model (eventEdit, cmds) =
  ( { model | page = EventEdit eventEdit}
  , Cmd.map EventEditMsg cmds
  )

stepEventCreate : Model -> ( EC.Model, Cmd EC.Msg ) -> ( Model, Cmd Msg )
stepEventCreate model (eventCreate, cmds) =
  ( { model | page = EventCreate eventCreate }
  , Cmd.map EventCreateMsg cmds
  )







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
                    case (model.session.idToken, model.page) of
                        ("", _) -> loginView model
                        (_, EventList el) -> Html.map EventListMsg (EL.view el)
                        (_, EventCreate ec) -> Html.map EventCreateMsg (EC.view ec)
                        (_, EventEdit ee) -> Html.map EventEditMsg (EE.view ee)
                        (_, RegisterRequired) -> registerInformationView model
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
