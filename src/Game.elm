module Game exposing (..)

import Browser
import Configuration.Control as ConfigurationControl exposing (toConfiguration, toDescription)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import LightsOut exposing (Configuration, Description, LightsOut, Mode(..), Solvable(..))
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
            { mode = Standard
            , columns = 5
            , rows = 5
            , colors = 2
            }

        puzzle =
            LightsOut.create description
    in
    ( { puzzle = puzzle
      , origin = puzzle
      , solvable = Solvable
      , control = ConfigurationControl.default description
      }
    , Cmd.none
    )


type alias Model =
    { puzzle : LightsOut
    , origin : LightsOut
    , solvable : Solvable
    , control : ConfigurationControl.Model
    }


type Msg
    = LightsOutMessage LightsOut.Msg
    | ConfigurationControlMessage ConfigurationControl.Msg
    | ClearPuzzle
    | RandomPuzzle
    | ResetPuzzle
    | ChangeMode Mode
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
                    LightsOut.create <| toDescription (LightsOut.modeOf model.puzzle) model.control
            in
            ( model, Task.perform SetPuzzle <| Task.succeed puzzle )

        RandomPuzzle ->
            ( model, Random.generate SetPuzzle (LightsOut.random <| toDescription (LightsOut.modeOf model.puzzle) model.control) )

        ResetPuzzle ->
            ( { model | puzzle = model.origin }, Cmd.none )

        ChangeMode mode ->
            ( { model | puzzle = LightsOut.changeModeTo mode model.puzzle }, Cmd.none )

        SetPuzzle puzzle ->
            ( { model | puzzle = puzzle, origin = puzzle, solvable = LightsOut.solvable puzzle }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.map ConfigurationControlMessage <| ConfigurationControl.view model.control
        , viewControls model
        , Html.map LightsOutMessage <| LightsOut.view (toConfiguration model.control) model.puzzle
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        solvable =
            case model.solvable of
                Solvable ->
                    "✓"

                Unsolvable ->
                    "⛌"

                Unknown ->
                    "?"
    in
    Html.div []
        [ Html.span [] [ Html.text solvable ]
        , Html.input [ Attribute.type_ "radio", Attribute.id "set", Attribute.name "mode", Attribute.checked <| LightsOut.modeOf model.puzzle == Set, Event.onInput <| \_ -> ChangeMode Set ] []
        , Html.label [ Attribute.for "set" ] [ Html.text "set" ]
        , Html.input [ Attribute.type_ "radio", Attribute.id "standard", Attribute.name "mode", Attribute.checked <| LightsOut.modeOf model.puzzle == Standard, Event.onInput <| \_ -> ChangeMode Standard ] []
        , Html.label [ Attribute.for "standard" ] [ Html.text "standard" ]
        , Html.input [ Attribute.type_ "radio", Attribute.id "restricted", Attribute.name "mode", Attribute.checked <| LightsOut.modeOf model.puzzle == Restricted, Event.onInput <| \_ -> ChangeMode Restricted ] []
        , Html.label [ Attribute.for "restricted" ] [ Html.text "restricted" ]
        , Html.button [ Event.onClick ClearPuzzle ] [ Html.text "clear" ]
        , Html.button [ Event.onClick RandomPuzzle ] [ Html.text "random" ]
        , Html.button [ Event.onClick ResetPuzzle ] [ Html.text "reset" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { control } =
    Sub.map ConfigurationControlMessage <| ConfigurationControl.subscriptions control
