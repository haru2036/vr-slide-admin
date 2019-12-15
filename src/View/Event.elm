module View.Event exposing (eventEditView, eventCreateView, slideEditRow, eventListView)
import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url.Builder as URLB
import String exposing(fromInt, toInt)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row 
import List

eventEditView : Model -> Html Msg
eventEditView model = case model.currentEvent of
                Just event -> div [] [input [onInput (\newName -> EventModified (NameChanged newName)), value event.name] []
                                    , button [ class "pure-button pure-button-primary", onClick <| EventModified AddSlide ] [text "Add"]
                                    , button [ class "pure-button pure-button-primary", onClick <| SaveEvent event] [text "Save"]
                                    , Button.button [ Button.danger, Button.attrs [onClick <| DeleteEvent event]] [text "Delete"]
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
slideEditRow index slide = Grid.container [ class "card"
                                          , style "background-image" ("url(\"" ++ firstPageUrl slide.sdid ++ "\")")
                                          , style "background-size" "contain"
                                          , style "background-repeat" "no-repeat"
                                          ] 
                               [ Grid.row [ Row.attrs <| [class "slide-edit-content"]] [
                                   Grid.col [] [
                                    div [] 
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

firstPageUrl : String -> String
firstPageUrl slideId = case slideId of 
    "" -> ""
    _ -> String.concat ["https://speakerd.s3.amazonaws.com/presentations/",
            slideId,
            "/preview_slide_0",
            ".jpg?373063"] 

eventListView : Model -> Html Msg
eventListView model = case model.events of
    [] -> div [class "container"] [text "empty"
                        , a [ class "pure-button pure-button-primary", href <| URLB.absolute["event", "new"] []] [ text "Create new"]
                        ]
    _ -> div [class "container" ] [ a
                                    [ class "pure-button pure-button-primary"
                                    , href <| URLB.absolute["event", "new"] []] [ text "Create new"]
                                  , model.events |> List.map (\ev -> div [ class "card" ] [a [href <| URLB.absolute ["event", "edit", ev.eventId] []] [text ev.name], div [] [(slideListView ev.slides)]]) |> div []]


slideListView : List Slide -> Html Msg
slideListView slides = slides |> List.map (\s -> ul [] [text s.sdid] ) |> div []
