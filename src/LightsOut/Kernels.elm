module LightsOut.Kernels exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Colors =
    Int


type alias Columns =
    Int


type alias Rows =
    Int


type alias Kernel =
    Array Int


type alias Base =
    List Kernel


type alias Kernels =
    Dict Colors (Dict Columns (Dict Rows Base))


base : Colors -> Columns -> Rows -> Maybe Base
base colors columns rows =
    knownKernels
        |> Dict.get colors
        |> Maybe.andThen (Dict.get columns)
        |> Maybe.andThen (Dict.get rows)


knownKernels : Kernels
knownKernels =
    empty
        |> add 2 1 1 (Array.fromList [])
        |> add 3 1 1 (Array.fromList [])
        |> add 4 1 1 (Array.fromList [])
        |> add 5 1 1 (Array.fromList [])
        |> add 6 1 1 (Array.fromList [])
        |> add 7 1 1 (Array.fromList [])
        |> add 8 1 1 (Array.fromList [])
        |> add 9 1 1 (Array.fromList [])
        |> add 10 1 1 (Array.fromList [])
        |> add 2 2 1 (Array.fromList [ 1, 1 ])
        |> add 3 2 1 (Array.fromList [ 1, 2 ])
        |> add 4 2 1 (Array.fromList [ 1, 3 ])
        |> add 5 2 1 (Array.fromList [ 1, 4 ])
        |> add 6 2 1 (Array.fromList [ 1, 5 ])
        |> add 7 2 1 (Array.fromList [ 1, 6 ])
        |> add 8 2 1 (Array.fromList [ 1, 7 ])
        |> add 9 2 1 (Array.fromList [ 1, 8 ])
        |> add 10 2 1 (Array.fromList [ 1, 9 ])
        |> add 2 3 1 (Array.fromList [])
        |> add 3 3 1 (Array.fromList [])
        |> add 4 3 1 (Array.fromList [])
        |> add 5 3 1 (Array.fromList [])
        |> add 6 3 1 (Array.fromList [])
        |> add 7 3 1 (Array.fromList [])
        |> add 8 3 1 (Array.fromList [])
        |> add 9 3 1 (Array.fromList [])
        |> add 10 3 1 (Array.fromList [])
        |> add 2 4 1 (Array.fromList [])
        |> add 3 4 1 (Array.fromList [])
        |> add 4 4 1 (Array.fromList [])
        |> add 5 4 1 (Array.fromList [])
        |> add 6 4 1 (Array.fromList [])
        |> add 7 4 1 (Array.fromList [])
        |> add 8 4 1 (Array.fromList [])
        |> add 9 4 1 (Array.fromList [])
        |> add 10 4 1 (Array.fromList [])
        |> add 2 5 1 (Array.fromList [ 1, 1, 0, 1, 1 ])
        |> add 3 5 1 (Array.fromList [ 1, 2, 0, 1, 2 ])
        |> add 4 5 1 (Array.fromList [ 1, 3, 0, 1, 3 ])
        |> add 5 5 1 (Array.fromList [ 1, 4, 0, 1, 4 ])
        |> add 6 5 1 (Array.fromList [ 1, 5, 0, 1, 5 ])
        |> add 7 5 1 (Array.fromList [ 1, 6, 0, 1, 6 ])
        |> add 8 5 1 (Array.fromList [ 1, 7, 0, 1, 7 ])
        |> add 9 5 1 (Array.fromList [ 1, 8, 0, 1, 8 ])
        |> add 10 5 1 (Array.fromList [ 1, 9, 0, 1, 9 ])
        |> add 2 6 1 (Array.fromList [])
        |> add 3 6 1 (Array.fromList [])
        |> add 4 6 1 (Array.fromList [])
        |> add 5 6 1 (Array.fromList [])
        |> add 6 6 1 (Array.fromList [])
        |> add 7 6 1 (Array.fromList [])
        |> add 8 6 1 (Array.fromList [])
        |> add 9 6 1 (Array.fromList [])
        |> add 10 6 1 (Array.fromList [])
        |> add 2 7 1 (Array.fromList [])
        |> add 3 7 1 (Array.fromList [])
        |> add 4 7 1 (Array.fromList [])
        |> add 5 7 1 (Array.fromList [])
        |> add 6 7 1 (Array.fromList [])
        |> add 7 7 1 (Array.fromList [])
        |> add 8 7 1 (Array.fromList [])
        |> add 9 7 1 (Array.fromList [])
        |> add 10 7 1 (Array.fromList [])
        |> add 2 8 1 (Array.fromList [ 1, 1, 0, 1, 1, 0, 1, 1 ])
        |> add 3 8 1 (Array.fromList [ 1, 2, 0, 1, 2, 0, 1, 2 ])
        |> add 4 8 1 (Array.fromList [ 1, 3, 0, 1, 3, 0, 1, 3 ])
        |> add 5 8 1 (Array.fromList [ 1, 4, 0, 1, 4, 0, 1, 4 ])
        |> add 6 8 1 (Array.fromList [ 1, 5, 0, 1, 5, 0, 1, 5 ])
        |> add 7 8 1 (Array.fromList [ 1, 6, 0, 1, 6, 0, 1, 6 ])
        |> add 8 8 1 (Array.fromList [ 1, 7, 0, 1, 7, 0, 1, 7 ])
        |> add 9 8 1 (Array.fromList [ 1, 8, 0, 1, 8, 0, 1, 8 ])
        |> add 10 8 1 (Array.fromList [ 1, 9, 0, 1, 9, 0, 1, 9 ])
        |> add 2 9 1 (Array.fromList [])
        |> add 3 9 1 (Array.fromList [])
        |> add 4 9 1 (Array.fromList [])
        |> add 5 9 1 (Array.fromList [])
        |> add 6 9 1 (Array.fromList [])
        |> add 7 9 1 (Array.fromList [])
        |> add 8 9 1 (Array.fromList [])
        |> add 9 9 1 (Array.fromList [])
        |> add 10 9 1 (Array.fromList [])
        |> add 2 5 5 (Array.fromList [ 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1 ])
        |> add 2 5 5 (Array.fromList [ 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1 ])


empty : Kernels
empty =
    Dict.empty


add : Colors -> Columns -> Rows -> Kernel -> Kernels -> Kernels
add color columns rows kernel kernels =
    kernels
        |> assymmetricAdd color columns rows kernel
        |> assymmetricAdd color rows columns kernel


assymmetricAdd : Colors -> Columns -> Rows -> Kernel -> Kernels -> Kernels
assymmetricAdd color columns rows entry kernels =
    let
        rowsUpdate ks =
            ks
                |> Maybe.map ((::) entry)
                |> Maybe.withDefault [ entry ]
                |> Just

        columnsUpdate rs =
            rs
                |> Maybe.withDefault Dict.empty
                |> Dict.update rows rowsUpdate
                |> Just

        colorsUpdate cs =
            cs
                |> Maybe.withDefault Dict.empty
                |> Dict.update columns columnsUpdate
                |> Just
    in
    kernels
        |> Dict.update color colorsUpdate
