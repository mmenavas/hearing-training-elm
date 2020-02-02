port module Main exposing (..)

import Browser
import Delay
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, padding, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
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
    Element.layout []
        (el
            [ centerX
            , centerY

            -- , width fill
            ]
            (column []
                [ el
                    [ Font.color (rgb255 64 64 64)
                    , Font.size 48
                    , centerX
                    , padding 8
                    ]
                    (text "Hearing Trainer")
                , viewMainContent model
                , paragraph []
                    [ text (viewStatus model.status) ]
                ]
            )
        )


viewMainContent : Model -> Element Msg
viewMainContent model =
    case model.screen of
        Home ->
            viewHome model

        Game ->
            viewGame model


viewHome : Model -> Element Msg
viewHome _ =
    el [ centerX ]
        (Input.button
            [ Background.color (rgb255 50 10 175)
            , Font.color (rgb255 255 255 255)
            , Border.rounded 3
            , padding 30
            ]
            { onPress = Just StartGame
            , label =
                text "Start Game"
            }
        )


viewGame : Model -> Element Msg
viewGame model =
    el [ centerX ]
        (column []
            [ el
                [ centerX
                , padding 8
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
            [ Background.color (rgb255 50 10 175)
            , Font.color (rgb255 255 255 255)
            , Border.rounded 3
            , padding 30
            ]
            { onPress = Just ListenToHiddenNote
            , label =
                text
                    (if model.show then
                        viewNote model.note

                     else
                        "?"
                    )
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
            { onPress = Just (MakeAGuess note)
            , label = text (viewNote note)
            }
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
