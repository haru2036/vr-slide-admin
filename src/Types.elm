module Types exposing (..)
import List

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