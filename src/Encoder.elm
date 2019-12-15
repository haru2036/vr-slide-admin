module Encoder exposing (eventsEncoder, eventEncoder, slideEncoder)
import Model exposing (..)
import Json.Encode as Encode
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