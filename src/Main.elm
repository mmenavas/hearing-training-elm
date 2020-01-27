module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (Element, el, text, column, row, alignRight, fill, width, rgb255, spacing, centerX, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Random
import Delay



---- MODEL ----


type alias Model =
    { screen: Screen
    , status: Status
    , note: Note
    , show: Bool
    }


type Screen
  = Home
  | Game


type Status
  = Start
  | Play
  | Match
  | Fail
  | Win


type Note
  = Do
  | Re
  | Mi
  | Fa
  | Sol
  | La
  | Si


init : ( Model, Cmd Msg )
init =
      ( Model Game Start Do True
      , Random.generate UpdateNote noteGenerator
      )


---- UPDATE ----


type Msg
    = PlayNote Note
    | Shuffle
    | UpdateNote Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayNote note ->
            if note == model.note then
                ( { model | show = True }
                , showMainNote
                )
            else
                ( model
                , Cmd.none
                )

        Shuffle ->
            (model
            , Random.generate UpdateNote noteGenerator
            )
            
        UpdateNote newNote ->
            ( Model
                model.screen
                model.status
                newNote 
                False
            , Cmd.none
            )


---- COMMANDS ----


showMainNote: Cmd Msg
showMainNote =
    Delay.after 500 Delay.Millisecond Shuffle


noteGenerator : Random.Generator Note
noteGenerator =
  Random.uniform Do
    [ Re
    , Mi
    , Fa
    , Sol
    , La
    , Si
    ]


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
                    ( mainNote model )
                , el
                    [ centerX
                    , padding 8
                    ]
                    ( row [ spacing 8 ]
                        [ musicNote Do
                        , musicNote Re
                        , musicNote Mi
                        , musicNote Fa
                        , musicNote Sol
                        , musicNote La
                        , musicNote Si
                        ]
                    )
                , el
                    [ centerX
                    , padding 8 
                    ]
                    ( statusMsg ( viewStatus model.status ) )
                ]
            )
        )


viewStatus : Status -> String
viewStatus status =
    case status of
        Start -> "Push the yellow buttons to become familiar with the sounds of the music notes. When you're ready, push the start button to beginn playing."
        Play -> "Push the card with the question mark to listen to a musical note, then select the correct note from the multiple choices."
        Match -> "Very well! Now guess the next note."
        Fail -> "Try again! That was not the correct note."
        Win -> "Excellent! You won."
     

viewNote : Note -> String
viewNote note =
    case note of
        Do -> "Do"
        Re -> "Re"
        Mi -> "Mi"
        Fa -> "Fa"
        Sol -> "Sol"
        La -> "La"
        Si -> "Si"


mainNote : Model -> Element Msg
mainNote model =
    el []
        (Input.button
            [ Background.color (rgb255 50 10 175)
            , Font.color (rgb255 255 255 255)
            , Border.rounded 3
            , padding 30
            ]
            { onPress = Just (PlayNote model.note)
            , label = text (if model.show then viewNote model.note else "?")
            }
        )


musicNote : Note -> Element Msg
musicNote note =
    el []
        (Input.button
            [ Background.color (rgb255 40 0 145)
            , Font.color (rgb255 255 255 255)
            , Border.rounded 3
            , padding 30
            ]
            { onPress = Just (PlayNote note)
            , label = text (viewNote note)
            }
        )


statusMsg : String -> Element Msg
statusMsg status =
    row []
        [ el []
            ( text status )
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
