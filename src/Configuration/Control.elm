module Configuration.Control exposing (Model, Msg, default, subscriptions, toConfiguration, toDescription, update, view)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import LightsOut exposing (Configuration, Description)
import SingleSlider as Slider


type alias Model =
    { columns : Slider.Model
    , rows : Slider.Model
    , colors : Slider.Model
    , width : Slider.Model
    , gap : Slider.Model
    , showContent : Bool
    , showControl : Bool
    }


default : Description -> Model
default description =
    let
        defaultSlider =
            Slider.defaultModel
    in
    { columns = { defaultSlider | min = 1, max = 20, step = 1, value = toFloat description.columns }
    , rows = { defaultSlider | min = 1, max = 20, step = 1, value = toFloat description.rows }
    , colors = { defaultSlider | min = 2, max = 10, step = 1, value = toFloat description.colors }
    , width = { defaultSlider | min = 100, max = 600, step = 50, value = 300 }
    , gap = { defaultSlider | min = 0.1, max = 0.9, step = 0.02, value = 0.4 }
    , showContent = False
    , showControl = False
    }


toDescription : Model -> Description
toDescription description =
    { columns = floor description.columns.value
    , rows = floor description.rows.value
    , colors = floor description.colors.value
    }


toConfiguration : Model -> Configuration
toConfiguration model =
    { width = model.width.value
    , gap = model.gap.value
    , showContent = model.showContent
    }


type ControlElement
    = Columns
    | Rows
    | Colors
    | Width
    | Gap


type Msg
    = Slider ControlElement Slider.Msg
    | ShowContent Bool
    | ToggleControl


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ToggleControl ->
            ( { model | showControl = not model.showControl }, Cmd.none )

        ShowContent showContent ->
            ( { model | showContent = showContent }, Cmd.none )

        Slider element msg ->
            let
                ( slider, cmd, _ ) =
                    case element of
                        Columns ->
                            Slider.update msg model.columns

                        Rows ->
                            Slider.update msg model.rows

                        Colors ->
                            Slider.update msg model.colors

                        Width ->
                            Slider.update msg model.width

                        Gap ->
                            Slider.update msg model.gap

                updatedModel =
                    case element of
                        Columns ->
                            { model | columns = slider }

                        Rows ->
                            { model | rows = slider }

                        Colors ->
                            { model | colors = slider }

                        Width ->
                            { model | width = slider }

                        Gap ->
                            { model | gap = slider }
            in
            ( updatedModel, Cmd.map (Slider element) cmd )


view : Model -> Html Msg
view control =
    if control.showControl then
        Html.div []
            [ Html.span [ Event.onClick ToggleControl ] [ Html.text "⏷" ]
            , Html.form []
                [ Html.label [ Attribute.for "columns" ] [ Html.text "columns" ]
                , Html.map (Slider Columns) (Html.fromUnstyled <| Slider.view control.columns)
                , Html.label [ Attribute.for "rows" ] [ Html.text "rows" ]
                , Html.map (Slider Rows) (Html.fromUnstyled <| Slider.view control.rows)
                , Html.label [ Attribute.for "colors" ] [ Html.text "colors" ]
                , Html.map (Slider Colors) (Html.fromUnstyled <| Slider.view control.colors)
                , Html.label [ Attribute.for "width" ] [ Html.text "width" ]
                , Html.map (Slider Width) (Html.fromUnstyled <| Slider.view control.width)
                , Html.label [ Attribute.for "gap" ] [ Html.text "gap" ]
                , Html.map (Slider Gap) (Html.fromUnstyled <| Slider.view control.gap)
                , Html.label [ Attribute.for "show" ] [ Html.text "show content" ]
                , Html.input [ Attribute.type_ "checkbox", Attribute.checked control.showContent, Event.onCheck ShowContent ] []
                ]
            , Html.hr [] []
            ]

    else
        Html.div []
            [ Html.span [ Event.onClick ToggleControl ] [ Html.text "⏵" ]
            , Html.hr [] []
            ]


subscriptions : Model -> Sub Msg
subscriptions control =
    Sub.batch
        [ Sub.map (Slider Columns) <| Slider.subscriptions control.columns
        , Sub.map (Slider Rows) <| Slider.subscriptions control.rows
        , Sub.map (Slider Colors) <| Slider.subscriptions control.colors
        , Sub.map (Slider Width) <| Slider.subscriptions control.width
        , Sub.map (Slider Gap) <| Slider.subscriptions control.gap
        ]
