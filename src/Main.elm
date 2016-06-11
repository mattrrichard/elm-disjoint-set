module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DisjointSet as DSet


main : Program Never
main =
    let
        initialModel =
            init 15 30
    in
        Html.beginnerProgram
            { model = initialModel
            , view = view
            , update = update
            }


type alias Model =
    { sets : DSet.DisjointSet
    , tiles : List (List Tile)
    , width : Int
    , height : Int
    , picked : Maybe Int
    }


type alias Tile =
    { id : Int
    , setId : Int
    }


init : Int -> Int -> Model
init w h =
    let
        newTile id =
            { id = id
            , setId = id
            }

        size =
            w * h

        makeRows : Int -> List a -> List (List a)
        makeRows k list =
            case list of
                [] ->
                    []

                _ ->
                    List.take k list :: makeRows k (List.drop k list)

        tiles =
            List.map newTile [0..(size - 1)]
                |> makeRows w
    in
        { sets = DSet.init size
        , tiles = tiles
        , width = w
        , height = h
        , picked = Nothing
        }


view : Model -> Html Action
view model =
    let
        cell tile =
            td []
                [ input
                    [ type' "button"
                    , onClick (Click tile.id)
                    , value <| toString tile.setId
                    ]
                    []
                ]

        row =
            tr [] << List.map cell
    in
        table []
            (List.map row model.tiles)


type Action
    = NoOp
    | Click Int


mapWithState : (a -> state -> ( a', state )) -> state -> List a -> ( List a', state )
mapWithState f state =
    let
        worker f x ( xs, state ) =
            let
                ( x', state' ) =
                    f x state
            in
                ( x' :: xs, state' )
    in
        List.foldr (worker f) ( [], state )


updateSetMembership : Model -> Model
updateSetMembership model =
    let
        updateTile tile sets =
            let
                ( setId, sets' ) =
                    DSet.find tile.id sets
            in
                ( { tile | setId = setId }, sets' )

        updateRow row sets =
            mapWithState updateTile sets row

        ( tiles, sets ) =
            mapWithState updateRow model.sets model.tiles
    in
        { model
            | tiles = tiles
            , sets = sets
        }


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Click tileId ->
            handleClick tileId model


handleClick : Int -> Model -> Model
handleClick tileId model =
    case ( tileId, model.picked ) of
        ( tileId, Nothing ) ->
            { model | picked = Just tileId }

        ( tileId, Just picked ) ->
            let
                model' =
                    { model | picked = Nothing }
            in
                if tileId == picked then
                    model'
                else
                    let
                        sets =
                            DSet.union tileId picked model.sets
                    in
                        updateSetMembership { model' | sets = sets }
