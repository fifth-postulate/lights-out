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
                { rows = 6
                , columns = 5
                , colors = 3
                }
    in
    ( { puzzle = puzzle }, Cmd.none )


type alias Model =
    { puzzle : LightsOut }


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
view { puzzle } =
    Html.map LightsOutMessage <| LightsOut.view puzzle


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
