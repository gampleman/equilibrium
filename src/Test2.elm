module Test2 exposing (main)

import Array exposing (Array)
import Browser
import Color
import Html exposing (Html, text)
import Html.Events
import Point2d exposing (Point2d)
import Random
import VoronoiDiagram2d


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { sites : Array Site
    , score : Int
    }


type alias Flags =
    {}


type Site
    = Site Point2d CellState


type CellState
    = Nature
    | Human


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { cellMap = initialCellMap 3445768976564
      , score = 10
      }
    , Cmd.none
    )


initialCellGrid : Int -> List Site
initialCellGrid seed =
    Random.step gridGenerator (Random.initialSeed seed)
        |> Tuple.first


gridGenerator : Random.Generator (List Site)
gridGenerator =
    Random.map Array.fromList
        (Random.list 10
            (Random.map3 (\x y state -> Site (Point2d.fromCoordinates x y) state)
                (Random.float 0 500)
                (Random.float 0 500)
                (Random.weighted ( 0.5, Nature ) [ ( 0.5, Human ) ])
            )
        )


view : Model -> Html Msg
view model =
    if model.score <= 0 then
        Html.text "YOU LOST! YOU SUCK!"

    else
        Html.div []
            [ VoronoiDiagram2d.fromPoints
            , Html.p [] [ text "Score ", text (String.fromInt model.score) ]
            ]


cellRenderer =
    { cellSize = 30
    , cellColorizer =
        \state ->
            case state of
                Nature ->
                    Color.rgb 0.1 0.9 0.1

                Human ->
                    Color.rgb 0.3 0.3 0.3
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0
    , gridLineColor = Color.rgb 0 0 1
    }


type Msg
    = GridMsg CellGrid.Render.Msg
    | Clicked ( Int, Int )
    | Evaluated (CellGrid CellState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked coords ->
            ( { model | cellMap = flipAtIndex coords model.cellMap }, Random.generate Evaluated (ruleGenerator model.cellMap) )

        Evaluated grid ->
            ( { model | cellMap = grid, score = modifyScore model }, Cmd.none )

        _ ->
            ( model, Cmd.none )


flipAtIndex : ( Int, Int ) -> CellGrid CellState -> CellGrid CellState
flipAtIndex coords grid =
    let
        state =
            CellGrid.cellAtMatrixIndex coords grid |> Maybe.withDefault Nature
    in
    CellGrid.setValue grid coords (flip state)


tolerance =
    { low = 3, high = 40 }


modifyScore : Model -> Int
modifyScore { score, cellMap } =
    let
        { natureCount, humanCount } =
            evaluateBalance cellMap

        diff =
            abs (humanCount - natureCount)
    in
    if diff <= tolerance.low then
        score + 1

    else if diff >= tolerance.high then
        score - 1

    else
        score


ruleGenerator : CellGrid CellState -> Random.Generator (CellGrid CellState)
ruleGenerator ((CellGrid size elements) as grid) =
    makeRule isolatedRule grid
        |> Random.andThen (makeRule neigborInfluenceRule)


traverse : CellGrid (Random.Generator CellState) -> Random.Generator (CellGrid CellState)
traverse ((CellGrid size elements) as grid) =
    grid
        |> CellGrid.foldl (\cell list -> Random.map2 (::) cell list) (Random.constant [])
        |> Random.map List.reverse
        |> Random.map (CellGrid.fromList size)


flip : CellState -> CellState
flip cell =
    case cell of
        Nature ->
            Human

        Human ->
            Nature


makeRule : (CellState -> ( Int, Int ) -> CellGrid CellState -> Random.Generator CellState) -> CellGrid CellState -> Random.Generator (CellGrid CellState)
makeRule rule grid =
    CellGrid.transform
        (\index grid_ ->
            let
                current =
                    CellGrid.cellAtMatrixIndex index grid_
                        |> Maybe.withDefault Nature
            in
            rule current index grid
        )
        grid
        |> traverse


isolatedRule : CellState -> ( Int, Int ) -> CellGrid CellState -> Random.Generator CellState
isolatedRule current index grid =
    if
        neighbors index grid
            |> List.all (\cell -> current /= cell)
    then
        Random.constant (flip current)

    else
        Random.constant current


neigborInfluenceRule : CellState -> ( Int, Int ) -> CellGrid CellState -> Random.Generator CellState
neigborInfluenceRule current index grid =
    let
        allNeigbors =
            neighbors index grid

        different =
            List.filter (\cell -> current /= cell) allNeigbors
                |> List.length
    in
    Random.weighted ( toFloat different, flip current ) [ ( List.length allNeigbors |> toFloat, current ) ]


neighbors : ( Int, Int ) -> CellGrid CellState -> List CellState
neighbors ( x, y ) grid =
    List.filterMap (\index -> CellGrid.cellAtMatrixIndex index grid)
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        ]


adjacent : ( Int, Int ) -> CellGrid CellState -> List CellState
adjacent ( x, y ) grid =
    List.filterMap (\index -> CellGrid.cellAtMatrixIndex index grid)
        [ ( x, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x, y + 1 )
        ]


evaluateBalance : CellGrid CellState -> { natureCount : Int, humanCount : Int }
evaluateBalance =
    CellGrid.foldl
        (\state totals ->
            case state of
                Nature ->
                    { totals | natureCount = totals.natureCount + 1 }

                Human ->
                    { totals | humanCount = totals.humanCount + 1 }
        )
        { natureCount = 0, humanCount = 0 }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
