module Main exposing (main)

import Browser
import CellGrid exposing (CellGrid(..))
import CellGrid.Render exposing (CellRenderer)
import Color
import Html exposing (Html, text)
import Html.Events
import Random
import Time
import TypedSvg exposing (line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke)
import TypedSvg.Attributes.InPx exposing (height, width, x, x1, x2, y, y1, y2)
import TypedSvg.Types exposing (Fill(..))


gridSize =
    30


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cellMap : CellGrid CellState
    , health : Int
    , survivedSoFar : Int
    }


type alias Flags =
    {}


type CellState
    = Nature
    | Human


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { cellMap = initialCellGrid 3445768976564
      , health = 10
      , survivedSoFar = 0
      }
    , Cmd.none
    )


initialCellGrid : Int -> CellGrid CellState
initialCellGrid seed =
    Random.step gridGenerator (Random.initialSeed seed)
        |> Tuple.first


gridGenerator : Random.Generator (CellGrid CellState)
gridGenerator =
    Random.map (\tiles -> CellGrid.fromList gridSize gridSize tiles |> Maybe.withDefault CellGrid.empty)
        (Random.list (gridSize ^ 2) (Random.weighted ( 0.55, Nature ) [ ( 0.5, Human ) ]))


indicatorWidth =
    400


view : Model -> Html Msg
view model =
    if model.health <= 0 then
        Html.text "YOU LOST!"

    else
        let
            { natureCount, humanCount } =
                evaluateBalance model.cellMap
        in
        Html.div []
            [ CellGrid.Render.renderAsHtml (gridSize * itemSize) (gridSize * itemSize) cellRenderer model.cellMap
                |> Html.map (\(CellGrid.Render.MouseClick coords _) -> Clicked coords)
            , Html.p [] [ text "Health ", text (String.fromInt model.health) ]
            , svg [ width indicatorWidth, height 30 ]
                [ rect [ x 0, width (toFloat natureCount / (toFloat gridSize ^ 2) * indicatorWidth), height 20, fill (Fill natureColor) ] []
                , rect [ x (toFloat natureCount / (toFloat gridSize ^ 2) * indicatorWidth), width (toFloat humanCount / (toFloat gridSize ^ 2) * indicatorWidth), height 20, fill (Fill humanColor) ] []
                , line [ x1 (indicatorWidth / 2), x2 (indicatorWidth / 2), y1 21, y2 30, stroke (Color.rgb 0 0 0) ] []
                ]
            , Html.p [] [ text (String.fromInt model.survivedSoFar) ]
            ]


itemSize =
    20


natureColor =
    Color.rgb 0.1 0.9 0.1


humanColor =
    Color.rgb 0.3 0.3 0.3


cellRenderer =
    { cellSize = itemSize
    , cellColorizer =
        \state ->
            case state of
                Nature ->
                    natureColor

                Human ->
                    humanColor
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0
    , gridLineColor = Color.rgb 0 0 1
    }


type Msg
    = Clicked ( Int, Int )
    | Evaluated (CellGrid CellState)
    | EndTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked coords ->
            ( { model | cellMap = flipAtIndex coords model.cellMap }, Cmd.none )

        EndTurn ->
            ( { model | survivedSoFar = model.survivedSoFar + 1 }, Random.generate Evaluated (ruleGenerator model.cellMap) )

        Evaluated grid ->
            ( { model | cellMap = grid, health = modifyHealth model }, Cmd.none )


flipAtIndex : ( Int, Int ) -> CellGrid CellState -> CellGrid CellState
flipAtIndex coords grid =
    let
        state =
            CellGrid.cellAtMatrixIndex coords grid |> Maybe.withDefault Nature
    in
    CellGrid.setValue grid coords (flip state)


tolerance =
    { low = gridSize ^ 2 * 0.02 |> floor, high = gridSize ^ 2 * 0.4 |> floor }


modifyHealth : Model -> Int
modifyHealth { health, cellMap } =
    let
        { natureCount, humanCount } =
            evaluateBalance cellMap

        diff =
            abs (humanCount - natureCount)
    in
    if diff <= tolerance.low then
        health + 1

    else if diff >= tolerance.high then
        health - 1

    else
        health


ruleGenerator : CellGrid CellState -> Random.Generator (CellGrid CellState)
ruleGenerator ((CellGrid size elements) as grid) =
    makeRule isolatedRule grid
        |> Random.andThen (makeRule neigborInfluenceRule)
        |> Random.andThen bombRule


traverse : CellGrid (Random.Generator CellState) -> Random.Generator (CellGrid CellState)
traverse ((CellGrid ( w, h ) elements) as grid) =
    grid
        |> CellGrid.foldl (\cell list -> Random.map2 (::) cell list) (Random.constant [])
        |> Random.map List.reverse
        |> Random.map (CellGrid.fromList w h >> Maybe.withDefault CellGrid.empty)


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


bombRule : CellGrid CellState -> Random.Generator (CellGrid CellState)
bombRule grid =
    Random.weighted ( 20, False ) [ ( 1, True ) ]
        |> Random.andThen
            (\shouldItHappen ->
                if shouldItHappen then
                    Random.map3 (triggerBomb grid)
                        (Random.int 0 (gridSize - 1))
                        (Random.int 0 (gridSize - 1))
                        (Random.int 5 (gridSize / 4 |> floor))

                else
                    Random.constant grid
            )


triggerBomb : CellGrid CellState -> Int -> Int -> Int -> CellGrid CellState
triggerBomb grid x y size =
    let
        { natureCount, humanCount } =
            evaluateBalance grid

        targetValue =
            if natureCount > humanCount then
                Nature

            else
                Human
    in
    List.range (max 0 (x - size)) (min gridSize (x + size))
        |> List.concatMap (\x2 -> List.map (Tuple.pair x2) (List.range (max 0 (y - size)) (min gridSize (y + size))))
        |> List.foldl (\coords grid_ -> CellGrid.setValue grid_ coords targetValue) grid


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
    Random.weighted ( toFloat different, flip current ) [ ( toFloat (List.length allNeigbors) * 15, current ) ]


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
    Time.every 1000 (always EndTurn)
