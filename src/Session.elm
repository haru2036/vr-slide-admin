module Session exposing (..)
import Types exposing (Event)
import Browser.Navigation as Nav
-- SESSION DATA


type alias Data =
  { events : Maybe (List Event)
  , idToken : String
  , key : Nav.Key
  }


empty : Nav.Key -> Data
empty key =
  Data Nothing "" key
