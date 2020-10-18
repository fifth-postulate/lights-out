module LightsOut exposing (Button, Description, LightsOut, Msg, create, press, set, update, view)

import Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event


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


create : Description -> LightsOut
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


view : LightsOut -> Html Msg
view ((Puzzle { columns, lights }) as puzzle) =
    let
        size =
            50

        offset =
            4

        w =
            columns
                * (size + offset)
                |> toFloat

        button =
            viewButton { size = size }
    in
    Html.div
        [ Attribute.css
            [ width (px w)
            , displayFlex
            , flexDirection row
            , flexWrap wrap
            , justifyContent spaceAround
            ]
        ]
    <|
        List.map button (Array.toIndexedList lights)


type alias ButtonConfiguration =
    { size : Float
    }


viewButton : ButtonConfiguration -> ( Int, Int ) -> Html Msg
viewButton configuration ( index, value ) =
    Html.span
        [ Attribute.css
            [ displayFlex
            , justifyContent center
            , alignItems center
            , width (px configuration.size)
            , height (px configuration.size)
            ]
        , Event.onClick (Pressed <| Button index)
        ]
        [ Html.text <| String.fromInt value ]


type Msg
    = Pressed Button


update : Msg -> LightsOut -> ( LightsOut, Cmd Msg )
update message puzzle =
    case message of
        Pressed b ->
            ( press b puzzle, Cmd.none )
