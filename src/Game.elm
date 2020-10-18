module Game exposing (..)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import LightsOut exposing (LightsOut)


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
        puzzle =
            LightsOut.create
                { rows = 3
                , columns = 3
                , colors = 3
                }
    in
    ( { puzzle = puzzle, width = 300, gap = 4 }, Cmd.none )


type alias Model =
    { puzzle : LightsOut
    , width : Float
    , gap : Float
    }


type Msg
    = LightsOutMessage LightsOut.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LightsOutMessage msg ->
            let
                ( puzzle, cmd ) =
                    LightsOut.update msg model.puzzle
            in
            ( { model | puzzle = puzzle }, Cmd.map LightsOutMessage cmd )


view : Model -> Html Msg
view model =
    Html.map LightsOutMessage <| LightsOut.view model model.puzzle


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
