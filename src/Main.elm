port module Main exposing (..)

import Browser
import Delay
import Element exposing (Color, Element, centerX, centerY, column, el, padding, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Encode as E
import List exposing (head, length, member)
import Random exposing (generate)
import Random.List exposing (choose)



---- PORTS ----


port play : E.Value -> Cmd msg



---- CUSTOM TYPES ----


type Screen
    = HomeScreen
    | PrepScreen
    | GameScreen


type Status
    = OnHome
    | PreparingForGame
    | StartingGame
    | MatchedNote
    | GuessNextNote
    | FailedToMatchNote
    | GameWon
    | GameLost


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
    , note : Maybe Note
    , reveal : Bool
    , matches : List Note
    , misses : Int
    , noteCount : Int
    , noteList : List Note
    }


init : Model
init =
    let
        noteList =
            [ Do, Re, Mi, Fa, Sol, La, Si ]
    in
    Model HomeScreen OnHome (head noteList) True [] 0 (length noteList) noteList


newGame : Model -> ( Model, Cmd Msg )
newGame model =
    ( { model | screen = GameScreen, status = StartingGame }
    , generate UpdateConcealedNote (choose model.noteList)
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
    | UpdateConcealedNote ( Maybe Note, List Note )
    | EndGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoHome ->
            ( init, Cmd.none )

        StartPrep ->
            ( { model | screen = PrepScreen, status = PreparingForGame }
            , Cmd.none
            )

        StartGame ->
            newGame init

        ListenToNote note ->
            ( model, playNote note )

        ListenToConcealedNote ->
            case model.note of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just mNote ->
                    ( model
                    , playNote mNote
                    )

        MakeAGuess note ->
            if not (model.status == GameWon || model.status == GameLost) then
                case model.note of
                    Nothing ->
                        ( model
                        , Cmd.none
                        )

                    Just mNote ->
                        if note == mNote then
                            let
                                newMatches =
                                    note :: model.matches
                            in
                            ( { model | reveal = True, status = MatchedNote, matches = newMatches }
                            , Cmd.batch [ playNote note, checkEndGame newMatches model.noteCount ]
                            )

                        else
                            ( { model | status = FailedToMatchNote, misses = model.misses + 1 }
                            , playNote note
                            )

            else
                ( model, playNote note )

        ShuffleConcealedNote ->
            ( model
            , generate UpdateConcealedNote (choose model.noteList)
            )

        UpdateConcealedNote ( newNote, newNoteList ) ->
            if model.status == StartingGame then
                ( { model | note = newNote, noteList = newNoteList, status = StartingGame, reveal = False }
                , Cmd.none
                )

            else
                ( { model | note = newNote, noteList = newNoteList, status = GuessNextNote, reveal = False }
                , Cmd.none
                )

        EndGame ->
            ( { model | status = GameWon, reveal = True }
            , Cmd.none
            )



---- COMMANDS ----


playNote : Note -> Cmd Msg
playNote note =
    play (E.string (noteToString note))


checkEndGame : List Note -> Int -> Cmd Msg
checkEndGame matches count =
    if length matches == count then
        Delay.after 1000 Delay.Millisecond EndGame

    else
        Delay.after 1000 Delay.Millisecond ShuffleConcealedNote



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color orange ]
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
                , Font.size 45
                , centerX
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
                            , paddingXY 36 20
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
                            , paddingXY 56 20
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
                        [ viewNote Do
                        , viewNote Re
                        , viewNote Mi
                        , viewNote Fa
                        ]
                    , row [ spacing 8, centerX ]
                        [ viewNote Sol
                        , viewNote La
                        , viewNote Si
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
                (viewNoteToGuess model)
            , el
                [ centerX
                , padding 8
                ]
                (column [ spacing 8 ]
                    [ row [ spacing 8, centerX ]
                        [ viewGuessingOption Do model.matches
                        , viewGuessingOption Re model.matches
                        , viewGuessingOption Mi model.matches
                        , viewGuessingOption Fa model.matches
                        ]
                    , row [ spacing 8, centerX ]
                        [ viewGuessingOption Sol model.matches
                        , viewGuessingOption La model.matches
                        , viewGuessingOption Si model.matches
                        ]
                    ]
                )
            , viewStatus model
            , viewGameNav model
            ]
        )


viewGameNav : Model -> Element Msg
viewGameNav model =
    if length model.matches == model.noteCount then
        el [ centerX, centerY ]
            (row [ spacing 20 ]
                [ el [ centerX ]
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
                , el [ centerX ]
                    (Input.button
                        [ Background.color orange
                        , Font.color white
                        , padding 10
                        ]
                        { onPress = Just StartGame
                        , label =
                            Element.html (Html.i [ Html.Attributes.class "fas fa-redo fa-2x" ] [])
                        }
                    )
                ]
            )

    else
        el [ centerX ]
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


viewStatus : Model -> Element Msg
viewStatus model =
    paragraph
        [ Font.color white
        , Font.size 16
        , width (px 312)
        , Element.htmlAttribute (Html.Attributes.style "marginLeft" "auto")
        , Element.htmlAttribute (Html.Attributes.style "marginRight" "auto")
        ]
        [ text (statusToString model.status) ]


viewNoteToGuess : Model -> Element Msg
viewNoteToGuess model =
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
                if model.reveal then
                    case model.note of
                        Nothing ->
                            text ""

                        Just mNote ->
                            text (noteToString mNote)

                else
                    Element.html (Html.i [ Html.Attributes.class "fas fa-question" ] [])
            }
        )


viewNote : Note -> Element Msg
viewNote note =
    el []
        (Input.button
            [ Background.color slateBlue
            , Font.color white
            , Border.rounded 50
            , padding 20
            ]
            { onPress = Just (ListenToNote note)
            , label = text (noteToString note)
            }
        )


viewGuessingOption : Note -> List Note -> Element Msg
viewGuessingOption note matches =
    let
        disable =
            member note matches
    in
    el []
        (Input.button
            [ Background.color (if not disable then slateBlue else blue)
            , Font.color white
            , Border.rounded 50
            , padding 20
            ]
            { onPress = if not disable then Just (MakeAGuess note) else Nothing
            , label = text (noteToString note)
            }
        )



---- Custom Type to String ----


statusToString : Status -> String
statusToString status =
    case status of
        OnHome ->
            "Press Listen to hear the music notes used in this game or press Play to start playing."

        PreparingForGame ->
            "Play the notes above to become familiar with their sound."

        StartingGame ->
            "Push the card with the question mark to listen to a musical note, then select the correct note from the multiple choices."

        MatchedNote ->
            "Very well!"

        GuessNextNote ->
            "Now guess the next note."

        FailedToMatchNote ->
            "Try again! That was not the correct note."

        GameWon ->
            "Excellent! You won."

        GameLost ->
            "You lost! Better luck next time."


noteToString : Note -> String
noteToString note =
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
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
