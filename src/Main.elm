port module Main exposing (..)

import Browser
import Delay
import Element exposing (Color, Element, alignRight, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Encode as E
import Random



---- PORTS ----


port play : E.Value -> Cmd msg



---- CUSTOM TYPES ----


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



---- MODEL ----


type alias Model =
    { screen : Screen
    , status : Status
    , note : Note
    , show : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Home Start Do True
    , Random.generate UpdateHiddenNote noteGenerator
    )



---- UPDATE ----


type Msg
    = StartGame
    | GoHome
    | ListenToHiddenNote
    | MakeAGuess Note
    | ShuffleHiddenNote
    | UpdateHiddenNote Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | screen = Game, status = Play }
            , Cmd.none
            )

        GoHome ->
            init

        ListenToHiddenNote ->
            ( model
            , playNote model.note
            )

        MakeAGuess note ->
            if note == model.note then
                ( { model | show = True, status = Match }
                , Cmd.batch [ showMainNote, playNote note ]
                )

            else
                ( { model | status = Fail }
                , playNote note
                )

        ShuffleHiddenNote ->
            ( model
            , Random.generate UpdateHiddenNote noteGenerator
            )

        UpdateHiddenNote newNote ->
            ( { model | note = newNote, show = False }
            , Cmd.none
            )



---- COMMANDS ----


playNote : Note -> Cmd Msg
playNote note =
    play (E.string (viewNote note))


showMainNote : Cmd Msg
showMainNote =
    Delay.after 1000 Delay.Millisecond ShuffleHiddenNote


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
    Element.layout
        [ Background.color orange
        ]
        (viewMainContent model)


viewMainContent : Model -> Element Msg
viewMainContent model =
    case model.screen of
        Home ->
            viewHome model

        Game ->
            viewGame model


viewHome : Model -> Element Msg
viewHome model =
    el
        [ centerX
        , centerY
        ]
        (column [ spacing 40 ]
            [ el
                [ Font.color white
                , Font.size 48
                , centerX
                , padding 8
                ]
                (text "Hearing Trainer")
            , el [ centerX ]
                (row [ spacing 8 ]
                    [ el []
                        (Input.button
                            [ Background.color slateBlue
                            , Font.color white
                            , Font.size 24
                            , Border.rounded 10
                            , paddingXY 60 20
                            ]
                            { onPress = Just StartGame
                            , label =
                                text "Play"
                            }
                        )
                    , el []
                        (Input.button
                            [ Background.color orange
                            , Font.color white
                            , Font.size 24
                            , Border.rounded 10
                            , Border.color white
                            , Border.width 1
                            , paddingXY 40 20
                            ]
                            { onPress = Just StartGame
                            , label =
                                text "Listen"
                            }
                        )
                    ]
                )
            , paragraph
                [ Font.color white
                , Font.size 16
                , width (px 400)
                ]
                [ text (viewStatus model.status) ]
            ]
        )


viewGame : Model -> Element Msg
viewGame model =
    el [ centerX, centerY ]
        (column [ spacing 20 ]
            [ el
                [ centerX
                , padding 20
                ]
                (mainNote model)
            , el
                [ centerX
                , padding 8
                ]
                (row [ spacing 8 ]
                    [ musicNote Do
                    , musicNote Re
                    , musicNote Mi
                    , musicNote Fa
                    , musicNote Sol
                    , musicNote La
                    , musicNote Si
                    ]
                )
            , el [ centerX ]
                (Input.button
                    [ Background.color orange
                    , Font.color white
                    , padding 10
                    ]
                    { onPress = Just GoHome
                    , label =
                        -- text "Home"
                        Element.html (Html.i [ Html.Attributes.class "fas fa-home" ] [])
                    }
                )
            ]
        )


viewStatus : Status -> String
viewStatus status =
    case status of
        Start ->
            "Push the yellow buttons to become familiar with the sounds of the music notes. When you're ready, push the start button to beginn playing."

        Play ->
            "Push the card with the question mark to listen to a musical note, then select the correct note from the multiple choices."

        Match ->
            "Very well! Now guess the next note."

        Fail ->
            "Try again! That was not the correct note."

        Win ->
            "Excellent! You won."


viewNote : Note -> String
viewNote note =
    case note of
        Do ->
            "Do"

        Re ->
            "Re"

        Mi ->
            "Mi"

        Fa ->
            "Fa"

        Sol ->
            "Sol"

        La ->
            "La"

        Si ->
            "Si"


mainNote : Model -> Element Msg
mainNote model =
    el []
        (Input.button
            [ Background.color orange
            , Border.rounded 10
            , Border.color white
            , Border.width 1
            , padding 40
            , Font.color white
            , Font.size 50
            ]
            { onPress = Just ListenToHiddenNote
            , label =
                if model.show then
                    text (viewNote model.note)

                else
                    Element.html (Html.i [ Html.Attributes.class "fas fa-question" ] [])
            }
        )


musicNote : Note -> Element Msg
musicNote note =
    el []
        (Input.button
            [ Background.color slateBlue
            , Font.color white
            , Border.rounded 50
            , padding 30
            ]
            { onPress = Just (MakeAGuess note)
            , label = text (viewNote note)
            }
        )



---- Colors ----


orange : Color
orange =
    rgb255 250 141 0


blue : Color
blue =
    rgb255 0 164 250


slateBlue : Color
slateBlue =
    rgb255 73 138 173


white : Color
white =
    rgb255 255 255 255



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
