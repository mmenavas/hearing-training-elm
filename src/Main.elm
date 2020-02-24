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
    , note : Note
    , reveal : Bool
    , matches : Int
    , misses : Int
    , notesCount : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model HomeScreen OnHome Do False 0 0 0
    , Cmd.none
    )

newGame : ( Model, Cmd Msg )
newGame =
    ( Model GameScreen StartingGame Do True 0 0 7
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
    | EndGame


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
            newGame

        ListenToNote note ->
            ( model, playNote note )

        ListenToConcealedNote ->
            ( model
            , playNote model.note
            )

        MakeAGuess note ->
            if not ( model.status == GameWon || model.status == GameLost ) then 
                if note == model.note then
                    let
                        newMatches =
                            model.matches + 1
                    in
                    ( { model | reveal = True, status = MatchedNote, matches = newMatches }
                    , Cmd.batch [ playNote note, checkEndGame newMatches model.notesCount ]
                    )

                else
                    ( { model | status = FailedToMatchNote, misses = model.misses + 1 }
                    , playNote note
                    )
            else
                ( model, playNote note )

        ShuffleConcealedNote ->
            ( model
            , Random.generate UpdateConcealedNote noteGenerator
            )

        UpdateConcealedNote newNote ->
            if model.status == StartingGame then
                ( { model | note = newNote, status = StartingGame, reveal = False }
                , Cmd.none
                )

            else 
                ( { model | note = newNote, status = GuessNextNote, reveal = False }
                , Cmd.none
                )

        EndGame ->
            ( { model | status = GameWon, reveal = True }
            , Cmd.none
            )



---- COMMANDS ----


playNote : Note -> Cmd Msg
playNote note =
    play (E.string (viewNote note))


checkEndGame : int -> int -> Cmd Msg
checkEndGame matches count =
    if matches == count then
        Delay.after 1000 Delay.Millisecond EndGame

    else
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
            , viewGameNav model
            ]
        )


viewGameNav : Model -> Element Msg
viewGameNav model =
    if model.matches == model.notesCount then
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
                if model.reveal then
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
            , padding 20
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
                , padding 20
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
