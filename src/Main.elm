module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (Element, el, text, column, row, alignRight, fill, width, rgb255, spacing, centerX, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



---- MODEL ----


type alias Model =
    { action: String
    , notes: List String
    }


init : ( Model, Cmd Msg )
init =
      ( Model
            "Play music note"
            [ "Do"
            , "Re"
            , "Mi"
            , "Fa"
            , "Sol"
            , "La"
            , "Si"
            ]
      , Cmd.none
      )


---- UPDATE ----


type Msg
    = PlayNote String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayNote note ->
            ( Model
                ("Playing " ++ note)
                model.notes
            , Cmd.none
            )


---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        ( el 
            [ centerX
            , centerY
            ]
            ( column []
                [ el
                    [ Font.color (rgb255 64 64 64)
                    , Font.size 48
                    , centerX
                    , padding 8
                    ]
                    ( text "Hearing Trainer" )
                , el
                    [ centerX
                    , padding 8
                    ]
                    ( musicNote "Do" )
                , el
                    [ padding 8
                    ]
                    ( musicNotes model )
                , el
                    [ centerX
                    , padding 8   
                    ]
                    ( statusMsg model )
                ]
            )
        )


musicNote : String -> Element Msg
musicNote note =
    el []
        (Input.button
            [ Background.color (rgb255 40 0 145)
            , Element.focused
                [ Background.color (rgb255 238 238 238) ]
            , Font.color (rgb255 255 255 255)
            , Border.rounded 3
            , padding 30
            ]
            { onPress = Just (PlayNote note)
            , label = text note
            }
        )


musicNotes: Model -> Element Msg
musicNotes model =
    row
        [ width fill, centerY, spacing 30 ]
        ( List.map musicNote model.notes )



statusMsg : Model -> Element Msg
statusMsg model =
    row []
        [ el
            []
            ( text model.action )
        ]

        
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
