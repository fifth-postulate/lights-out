module Game exposing (..)

import Browser
import Configuration.Control as ConfigurationControl exposing (toDescription)
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
      , control =
            { columns = { defaultSlider | min = 1, max = 20, step = 1, value = columns }
            , rows = { defaultSlider | min = 1, max = 20, step = 1, value = rows }
            , colors = { defaultSlider | min = 2, max = 10, step = 1, value = colors }
            , width = { defaultSlider | min = 100, max = 600, step = 50, value = 300 }
            , gap = { defaultSlider | min = 0.1, max = 0.9, step = 0.02, value = 0.4 }
            , showContent = False
            , showControl = False
            }
      }
    , Cmd.none
    )


type alias Model =
    { puzzle : LightsOut
    , origin : LightsOut
    , control : ConfigurationControl.Model
    }


type Msg
    = LightsOutMessage LightsOut.Msg
    | ConfigurationControlMessage ConfigurationControl.Msg
    | ClearPuzzle
    | RandomPuzzle
    | ResetPuzzle
    | ToggleMode
    | SetPuzzle LightsOut


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LightsOutMessage msg ->
            let
                ( puzzle, cmd ) =
                    LightsOut.update msg model.puzzle
            in
            ( { model | puzzle = puzzle }, Cmd.map LightsOutMessage cmd )

        ConfigurationControlMessage msg ->
            let
                ( control, cmd ) =
                    ConfigurationControl.update msg model.control
            in
            ( { model | control = control }
            , Cmd.batch
                [ Cmd.map ConfigurationControlMessage cmd
                , Task.perform identity <| Task.succeed ClearPuzzle
                ]
            )

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


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map ConfigurationControlMessage <| ConfigurationControl.view model.control
        , viewControls model
        , Html.map LightsOutMessage <| LightsOut.view (toConfiguration model) model.puzzle
        ]


toConfiguration : Model -> Configuration
toConfiguration model =
    { width = model.control.width.value
    , gap = model.control.gap.value
    , showContent = model.control.showContent
    }


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
    Sub.map ConfigurationControlMessage <| ConfigurationControl.subscriptions control
