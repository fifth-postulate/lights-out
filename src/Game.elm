module Game exposing (..)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import LightsOut exposing (LightsOut)
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
            3

        rows =
            3

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
      , width = 300
      , gap = 0.4
      , showContent = False
      , columns = columns
      , rows = rows
      , colors = colors
      , showDescription = True
      , description =
            { column = { defaultSlider | min = 1, max = 20, step = 1, value = columns }
            , row = { defaultSlider | min = 1, max = 20, step = 1, value = rows }
            , colors = { defaultSlider | min = 1, max = 10, step = 1, value = colors }
            }
      }
    , Cmd.none
    )


type alias Model =
    { puzzle : LightsOut
    , width : Float
    , gap : Float
    , showContent : Bool
    , columns : Int
    , rows : Int
    , colors : Int
    , showDescription : Bool
    , description :
        { column : Slider.Model
        , row : Slider.Model
        , colors : Slider.Model
        }
    }


type Msg
    = LightsOutMessage LightsOut.Msg
    | UpdateDescription Description Int
    | Slider Description Slider.Msg
    | ToggleDescription


type Description
    = Columns
    | Rows
    | Colors


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToggleDescription ->
            ( { model | showDescription = not model.showDescription }, Cmd.none )

        LightsOutMessage msg ->
            let
                ( puzzle, cmd ) =
                    LightsOut.update msg model.puzzle
            in
            ( { model | puzzle = puzzle }, Cmd.map LightsOutMessage cmd )

        UpdateDescription description value ->
            let
                updatedModel =
                    case description of
                        Columns ->
                            { model | columns = value }

                        Rows ->
                            { model | rows = value }

                        Colors ->
                            { model | colors = value }
            in
            ( { updatedModel | puzzle = LightsOut.create updatedModel }, Cmd.none )

        Slider description msg ->
            let
                ( slider, cmd, _ ) =
                    case description of
                        Columns ->
                            Slider.update msg model.description.column

                        Rows ->
                            Slider.update msg model.description.row

                        Colors ->
                            Slider.update msg model.description.colors

                aDescription =
                    model.description

                updatedDescription =
                    case description of
                        Columns ->
                            { aDescription | column = slider }

                        Rows ->
                            { aDescription | row = slider }

                        Colors ->
                            { aDescription | colors = slider }

                task =
                    Task.succeed <| UpdateDescription description (floor slider.value)
            in
            ( { model | description = updatedDescription }
            , Cmd.batch
                [ Cmd.map (Slider description) cmd
                , Task.perform identity task
                ]
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewDescription model
        , Html.map LightsOutMessage <| LightsOut.view model model.puzzle
        ]


viewDescription : Model -> Html Msg
viewDescription { showDescription, columns, rows, colors, description } =
    if showDescription then
        Html.div []
            [ Html.span [ Event.onClick ToggleDescription ] [ Html.text "⏷" ]
            , Html.form []
                [ Html.label [ Attribute.for "columns" ] [ Html.text "columns" ]
                , Html.map (Slider Columns) (Html.fromUnstyled <| Slider.view description.column)
                , Html.label [ Attribute.for "rows" ] [ Html.text "rows" ]
                , Html.map (Slider Rows) (Html.fromUnstyled <| Slider.view description.row)
                , Html.label [ Attribute.for "colors" ] [ Html.text "colors" ]
                , Html.map (Slider Colors) (Html.fromUnstyled <| Slider.view description.colors)
                ]
            , Html.hr [] []
            ]

    else
        Html.div []
            [ Html.span [ Event.onClick ToggleDescription ] [ Html.text "⏵" ]
            , Html.hr [] []
            ]


subscriptions : Model -> Sub Msg
subscriptions { description } =
    Sub.batch
        [ Sub.map (Slider Columns) <| Slider.subscriptions description.column
        , Sub.map (Slider Rows) <| Slider.subscriptions description.row
        , Sub.map (Slider Colors) <| Slider.subscriptions description.colors
        ]
