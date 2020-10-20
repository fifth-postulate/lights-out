module Game exposing (..)

import Browser
import Configuration.Control as ConfigurationControl exposing (toConfiguration, toDescription)
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
        description =
            { columns = 5
            , rows = 5
            , colors = 2
            }

        puzzle =
            LightsOut.create description
    in
    ( { puzzle = puzzle
      , origin = puzzle
      , control = ConfigurationControl.default description
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
        , Html.map LightsOutMessage <| LightsOut.view (toConfiguration model.control) model.puzzle
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
    Sub.map ConfigurationControlMessage <| ConfigurationControl.subscriptions control
