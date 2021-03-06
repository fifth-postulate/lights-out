module LightsOut exposing (Button, Configuration, Description, LightsOut, Mode(..), Msg, Solvable(..), changeModeTo, create, modeOf, press, random, set, solvable, update, view)

import Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import LightsOut.Kernels as Kernels
import Random exposing (Generator)
import Random.Array as Rnd


type LightsOut
    = Puzzle State


type alias State =
    { mode : Mode
    , colors : Int
    , rows : Int
    , columns : Int
    , lights : Array Int
    }


type Mode
    = Standard
    | Restricted
    | Set


modeOf : LightsOut -> Mode
modeOf (Puzzle { mode }) =
    mode


changeModeTo : Mode -> LightsOut -> LightsOut
changeModeTo mode (Puzzle puzzle) =
    Puzzle { puzzle | mode = mode }


type alias Description =
    { colors : Int
    , rows : Int
    , columns : Int
    , mode : Mode
    }


create : Description -> LightsOut
create description =
    let
        n =
            description.rows * description.columns
    in
    Puzzle
        { mode = description.mode
        , colors = description.colors
        , rows = description.rows
        , columns = description.columns
        , lights = Array.repeat n 0
        }


random : Description -> Generator LightsOut
random description =
    let
        n =
            description.rows * description.columns

        range =
            Random.int 0 (description.colors - 1)

        toPuzzle =
            Puzzle << State description.mode description.colors description.rows description.columns
    in
    Rnd.array n range
        |> Random.map toPuzzle


type Button
    = Button Int


lit : Button -> LightsOut -> Basics.Bool
lit (Button index) (Puzzle { lights }) =
    Array.get index lights
        |> Maybe.map ((/=) 0)
        |> Maybe.withDefault False


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


type Solvable
    = Solvable
    | Unsolvable
    | Unknown


toSolvable : Bool -> Solvable
toSolvable v =
    case v of
        True ->
            Solvable

        False ->
            Unsolvable


solvable : LightsOut -> Solvable
solvable (Puzzle state) =
    let
        base =
            Kernels.base state.colors state.columns state.rows

        inproduct0 b =
            0 == inproduct state.colors state.lights b
    in
    base
        |> Maybe.map (List.all inproduct0)
        |> Maybe.map toSolvable
        |> Maybe.withDefault Unknown


inproduct : Int -> Array Int -> Array Int -> Int
inproduct n left right =
    let
        product ( l, r ) =
            modBy n (l * r)

        sum acc v =
            modBy n (acc + v)
    in
    zip (Array.toList left) (Array.toList right)
        |> List.map product
        |> List.foldl sum 0


zip : List a -> List b -> List ( a, b )
zip left right =
    case ( left, right ) of
        ( [], [] ) ->
            []

        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( l :: ls, r :: rs ) ->
            ( l, r ) :: zip ls rs


type alias Configuration =
    { width : Float
    , gap : Float
    , showContent : Bool
    }


view : Configuration -> LightsOut -> Html Msg
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
update message ((Puzzle { mode }) as puzzle) =
    case message of
        Pressed b ->
            case mode of
                Set ->
                    ( set b puzzle, Cmd.none )

                Standard ->
                    ( press b puzzle, Cmd.none )

                Restricted ->
                    if lit b puzzle then
                        ( press b puzzle, Cmd.none )

                    else
                        ( puzzle, Cmd.none )
