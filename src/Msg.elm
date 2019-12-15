module Msg exposing (..)
import Model exposing (..)
import Browser
import Url
import Url.Builder as URLB
import Url.Parser as URLP exposing((</>))
import Http exposing (Error(..))

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
    | DeleteEvent Event
    | CreateEvent Event
    | SavedEvent (Result Http.Error Event)
    | DeletedEvent (Result Http.Error ())
    | NoOp

type ModifyAction = Swap Int Int
                  | ChangeSlide Int Slide
                  | AddSlide 
                  | DeleteSlide Int
                  | NameChanged String

type Page = Login
          | Events
          | EventEdit