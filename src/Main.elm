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
    = HomeScreen
    | PrepScreen
    | GameScreen


type Status
    = PreparingForGame
    | StartingGame
    | PlayingGame
    | MatchedNote
    | FailedToMatchNote
    | WonTheGame


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
    ( Model HomeScreen StartingGame Do True
    , Random.generate UpdateConcealedNote noteGenerator
    )



---- UPDATE ----


type Msg
    = GoHome
    | StartPrep
    | StartGame
    | ListenToNote Note
    | ListenToConcealedNote
    | MakeAGuess Note
    | ShuffleConcealedNote
    | UpdateConcealedNote Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoHome ->
            init

        StartPrep ->
            ( { model | screen = PrepScreen, status = PreparingForGame }
            , Cmd.none
            )

        StartGame ->
            ( { model | screen = GameScreen, status = PlayingGame }
            , Cmd.none
            )

        ListenToNote note ->
            ( model, playNote note )

        ListenToConcealedNote ->
            ( model
            , playNote model.note
            )

        MakeAGuess note ->
            if note == model.note then
                ( { model | show = True, status = MatchedNote }
                , Cmd.batch [ revealNote, playNote note ]
                )

            else
                ( { model | status = FailedToMatchNote }
                , playNote note
                )

        ShuffleConcealedNote ->
            ( model
            , Random.generate UpdateConcealedNote noteGenerator
            )

        UpdateConcealedNote newNote ->
            ( { model | note = newNote, show = False }
            , Cmd.none
            )



---- COMMANDS ----


playNote : Note -> Cmd Msg
playNote note =
    play (E.string (viewNote note))


revealNote : Cmd Msg
revealNote =
    Delay.after 1000 Delay.Millisecond ShuffleConcealedNote


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

        -- , height fill
        -- , width fill
        ]
        (viewMainContent model)


viewMainContent : Model -> Element Msg
viewMainContent model =
    case model.screen of
        HomeScreen ->
            viewHome model

        PrepScreen ->
            viewNotes model

        GameScreen ->
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
                            [ Background.color orange
                            , Font.color white
                            , Font.size 24
                            , Border.rounded 10
                            , Border.color white
                            , Border.width 1
                            , paddingXY 40 20
                            ]
                            { onPress = Just StartPrep
                            , label =
                                text "Listen"
                            }
                        )
                    , el []
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
                    ]
                )
            , viewStatus model
            ]
        )


viewNotes : Model -> Element Msg
viewNotes model =
    el [ centerX, centerY ]
        (column [ spacing 20 ]
            [ el
                [ centerX
                , padding 8
                ]
                (column [ spacing 8 ]
                    [ row [ spacing 8, centerX ]
                        [ musicNote Do
                        , musicNote Re
                        , musicNote Mi
                        , musicNote Fa
                        ]
                    , row [ spacing 8, centerX ]
                        [ musicNote Sol
                        , musicNote La
                        , musicNote Si
                        ]
                    ]
                )
            , viewStatus model
            , el [ centerX ]
                (Input.button
                    [ Background.color orange
                    , Font.color white
                    , padding 10
                    ]
                    { onPress = Just GoHome
                    , label =
                        Element.html (Html.i [ Html.Attributes.class "fas fa-arrow-circle-left fa-2x" ] [])
                    }
                )
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
                (noteToBeGuessed model)
            , el
                [ centerX
                , padding 8
                ]
                (column [ spacing 8 ]
                    [ row [ spacing 8, centerX ]
                        [ guessingOption Do
                        , guessingOption Re
                        , guessingOption Mi
                        , guessingOption Fa
                        ]
                    , row [ spacing 8, centerX ]
                        [ guessingOption Sol
                        , guessingOption La
                        , guessingOption Si
                        ]
                    ]
                )
            , viewStatus model
            , el [ centerX ]
                (Input.button
                    [ Background.color orange
                    , Font.color white
                    , padding 10
                    ]
                    { onPress = Just GoHome
                    , label =
                        Element.html (Html.i [ Html.Attributes.class "fas fa-arrow-circle-left fa-2x" ] [])
                    }
                )
            ]
        )


statusToString : Status -> String
statusToString status =
    case status of
        PreparingForGame ->
            "Play the notes above to become familiar with their sound."

        StartingGame ->
            "Press Listen to hear the music notes used in this game or press Play to start playing."

        PlayingGame ->
            "Push the card with the question mark to listen to a musical note, then select the correct note from the multiple choices."

        MatchedNote ->
            "Very well! Now guess the next note."

        FailedToMatchNote ->
            "Try again! That was not the correct note."

        WonTheGame ->
            "Excellent! You won."


viewStatus : Model -> Element Msg
viewStatus model =
    paragraph
        [ Font.color white
        , Font.size 16
        , width (px 320)
        , Element.htmlAttribute (Html.Attributes.style "marginLeft" "auto")
        , Element.htmlAttribute (Html.Attributes.style "marginRight" "auto")
        ]
        [ text (statusToString model.status) ]


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


noteToBeGuessed : Model -> Element Msg
noteToBeGuessed model =
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
            { onPress = Just ListenToConcealedNote
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
            { onPress = Just (ListenToNote note)
            , label = text (viewNote note)
            }
        )


guessingOption : Note -> Element Msg
guessingOption note =
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
