module LightsOut exposing (Button, Configuration, LightsOut, Msg, create, press, random, set, update, view)

import Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Random exposing (Generator)
import Random.Array as Rnd


type LightsOut
    = Puzzle State


type alias State =
    { colors : Int
    , rows : Int
    , columns : Int
    , lights : Array Int
    }


type alias Description =
    { colors : Int
    , rows : Int
    , columns : Int
    }


create : { a | colors : Int, rows : Int, columns : Int } -> LightsOut
create description =
    let
        n =
            description.rows * description.columns
    in
    Puzzle
        { colors = description.colors
        , rows = description.rows
        , columns = description.columns
        , lights = Array.repeat n 0
        }


random : { a | colors : Int, rows : Int, columns : Int } -> Generator LightsOut
random description =
    let
        n =
            description.rows * description.columns

        range =
            Random.int 0 (description.colors - 1)

        toPuzzle =
            Puzzle << State description.colors description.rows description.columns
    in
    Rnd.array n range
        |> Random.map toPuzzle


type Button
    = Button Int


set : Button -> LightsOut -> LightsOut
set (Button index) ((Puzzle { columns }) as puzzle) =
    increment index puzzle


press : Button -> LightsOut -> LightsOut
press (Button index) ((Puzzle { columns }) as puzzle) =
    let
        rowNeighbour =
            if modBy columns index == 0 then
                [ index + 1 ]

            else if modBy columns index == columns - 1 then
                [ index - 1 ]

            else
                [ index - 1, index + 1 ]

        indices =
            List.concat
                [ rowNeighbour
                , [ index - columns, index, index + columns ]
                ]
    in
    List.foldl increment puzzle indices


increment : Int -> LightsOut -> LightsOut
increment index ((Puzzle ({ lights, colors } as state)) as puzzle) =
    let
        inc i =
            if i + 1 /= colors then
                i + 1

            else
                0
    in
    lights
        |> Array.get index
        |> Maybe.map inc
        |> Maybe.map (\v -> Array.set index v lights)
        |> Maybe.map (\updatedLights -> { state | lights = updatedLights })
        |> Maybe.map Puzzle
        |> Maybe.withDefault puzzle


type alias Configuration =
    { width : Float
    , gap : Float
    }


view : { a | width : Float, gap : Float, showContent : Bool } -> LightsOut -> Html Msg
view configuration ((Puzzle { colors, rows, columns, lights }) as puzzle) =
    let
        buttonSize =
            configuration.width / (configuration.gap + toFloat columns)

        gapSize =
            (configuration.width - toFloat columns * buttonSize) / toFloat columns

        w =
            toFloat columns
                * (buttonSize + gapSize)

        h =
            toFloat rows
                * (buttonSize + gapSize)

        button =
            viewButton { size = buttonSize, colors = colors, showContent = configuration.showContent }
    in
    Html.div
        [ Attribute.css
            [ width (px w)
            , height (px h)
            , displayFlex
            , flexDirection row
            , flexWrap wrap
            , justifyContent spaceBetween
            , property "align-content" "space-between"
            ]
        ]
    <|
        List.map button (Array.toIndexedList lights)


type alias ButtonConfiguration =
    { size : Float
    , colors : Int
    , showContent : Bool
    }


viewButton : ButtonConfiguration -> ( Int, Int ) -> Html Msg
viewButton configuration ( index, value ) =
    let
        colorAngle =
            if value == 0 then
                0.0

            else
                360 * (toFloat <| (value - 1)) / toFloat configuration.colors

        saturation =
            if value == 0 then
                0.0

            else
                1.0
    in
    Html.span
        [ Attribute.css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , borderRadius (pct 25)
            , width (px configuration.size)
            , height (px configuration.size)
            , backgroundColor <| hsl colorAngle saturation 0.5
            ]
        , Event.onClick (Pressed <| Button index)
        ]
        [ Html.text <|
            if configuration.showContent then
                String.fromInt value

            else
                ""
        ]


type Msg
    = Pressed Button


update : Msg -> LightsOut -> ( LightsOut, Cmd Msg )
update message puzzle =
    case message of
        Pressed b ->
            ( press b puzzle, Cmd.none )
