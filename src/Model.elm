module Model exposing (Model, Event, Slide)

import Browser.Navigation as Nav
import Url
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
