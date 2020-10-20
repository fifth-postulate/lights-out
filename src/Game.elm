module Game exposing (..)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import LightsOut exposing (Configuration, Description, LightsOut, Mode(..))
import Random
import SingleSlider as Slider
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        columns =
            5

        rows =
            5

        colors =
            2

        puzzle =
            LightsOut.create
                { columns = columns
                , rows = rows
                , colors = colors
                }

        defaultSlider =
            Slider.defaultModel
    in
    ( { puzzle = puzzle
      , origin = puzzle
      , gap = 0.4
      , showContent = False
      , showControl = False
      , control =
            { columns = { defaultSlider | min = 1, max = 20, step = 1, value = columns }
            , rows = { defaultSlider | min = 1, max = 20, step = 1, value = rows }
            , colors = { defaultSlider | min = 2, max = 10, step = 1, value = colors }
            , width = { defaultSlider | min = 100, max = 600, step = 50, value = 300 }
            }
      }
    , Cmd.none
    )


type alias Model =
    { puzzle : LightsOut
    , origin : LightsOut
    , gap : Float
    , showContent : Bool
    , showControl : Bool
    , control : Control
    }


type alias Control =
    { columns : Slider.Model
    , rows : Slider.Model
    , colors : Slider.Model
    , width : Slider.Model
    }


type ControlElement
    = Columns
    | Rows
    | Colors
    | Width


type Msg
    = LightsOutMessage LightsOut.Msg
    | ClearPuzzle
    | RandomPuzzle
    | ResetPuzzle
    | ToggleMode
    | SetPuzzle LightsOut
    | Slider ControlElement Slider.Msg
    | ToggleDescription


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToggleDescription ->
            ( { model | showControl = not model.showControl }, Cmd.none )

        LightsOutMessage msg ->
            let
                ( puzzle, cmd ) =
                    LightsOut.update msg model.puzzle
            in
            ( { model | puzzle = puzzle }, Cmd.map LightsOutMessage cmd )

        ClearPuzzle ->
            let
                puzzle =
                    LightsOut.create <| toDescription model.control
            in
            ( model, Task.perform SetPuzzle <| Task.succeed puzzle )

        RandomPuzzle ->
            ( model, Random.generate SetPuzzle (LightsOut.random <| toDescription model.control) )

        ResetPuzzle ->
            ( { model | puzzle = model.origin }, Cmd.none )

        ToggleMode ->
            ( { model | puzzle = toggle model.puzzle }, Cmd.none )

        SetPuzzle puzzle ->
            ( { model | puzzle = puzzle, origin = puzzle }, Cmd.none )

        Slider description msg ->
            let
                ( slider, cmd, _ ) =
                    case description of
                        Columns ->
                            Slider.update msg model.control.columns

                        Rows ->
                            Slider.update msg model.control.rows

                        Colors ->
                            Slider.update msg model.control.colors

                        Width ->
                            Slider.update msg model.control.width

                aControl =
                    model.control

                updatedControl =
                    case description of
                        Columns ->
                            { aControl | columns = slider }

                        Rows ->
                            { aControl | rows = slider }

                        Colors ->
                            { aControl | colors = slider }

                        Width ->
                            { aControl | width = slider }

                task =
                    Task.succeed ClearPuzzle
            in
            ( { model | control = updatedControl }
            , Cmd.batch
                [ Cmd.map (Slider description) cmd
                , Task.perform identity task
                ]
            )


toggle : LightsOut -> LightsOut
toggle puzzle =
    let
        mode =
            case LightsOut.modeOf puzzle of
                Play ->
                    Set

                _ ->
                    Play
    in
    LightsOut.changeModeTo mode puzzle


toDescription : Control -> Description
toDescription description =
    { columns = floor description.columns.value
    , rows = floor description.rows.value
    , colors = floor description.colors.value
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ viewDescription model
        , viewControls model
        , Html.map LightsOutMessage <| LightsOut.view (toConfiguration model) model.puzzle
        ]


toConfiguration : Model -> Configuration
toConfiguration model =
    { width = model.control.width.value
    , gap = model.gap
    , showContent = model.showContent
    }


viewDescription : Model -> Html Msg
viewDescription { showControl, control } =
    if showControl then
        Html.div []
            [ Html.span [ Event.onClick ToggleDescription ] [ Html.text "⏷" ]
            , Html.form []
                [ Html.label [ Attribute.for "columns" ] [ Html.text "columns" ]
                , Html.map (Slider Columns) (Html.fromUnstyled <| Slider.view control.columns)
                , Html.label [ Attribute.for "rows" ] [ Html.text "rows" ]
                , Html.map (Slider Rows) (Html.fromUnstyled <| Slider.view control.rows)
                , Html.label [ Attribute.for "colors" ] [ Html.text "colors" ]
                , Html.map (Slider Colors) (Html.fromUnstyled <| Slider.view control.colors)
                , Html.label [ Attribute.for "width" ] [ Html.text "width" ]
                , Html.map (Slider Width) (Html.fromUnstyled <| Slider.view control.width)
                ]
            , Html.hr [] []
            ]

    else
        Html.div []
            [ Html.span [ Event.onClick ToggleDescription ] [ Html.text "⏵" ]
            , Html.hr [] []
            ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div []
        [ Html.input [ Attribute.type_ "checkbox", Attribute.id "play", Attribute.checked <| LightsOut.modeOf model.puzzle == Play, Event.onInput <| \_ -> ToggleMode ] []
        , Html.label [ Attribute.for "play" ] [ Html.text "play" ]
        , Html.button [ Event.onClick ClearPuzzle ] [ Html.text "clear" ]
        , Html.button [ Event.onClick RandomPuzzle ] [ Html.text "random" ]
        , Html.button [ Event.onClick ResetPuzzle ] [ Html.text "reset" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { control } =
    Sub.batch
        [ Sub.map (Slider Columns) <| Slider.subscriptions control.columns
        , Sub.map (Slider Rows) <| Slider.subscriptions control.rows
        , Sub.map (Slider Colors) <| Slider.subscriptions control.colors
        , Sub.map (Slider Width) <| Slider.subscriptions control.width
        ]
