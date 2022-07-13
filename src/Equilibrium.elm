port module Equilibrium exposing (main)

import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import DelaunayTriangulation2d exposing (DelaunayTriangulation2d)
import Dict exposing (Dict)
import Force exposing (Force)
import Geometry.Svg
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (custom)
import Json.Decode as D exposing (Decoder, Value)
import Json.Encode
import Path
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Random
import Set exposing (Set)
import Shape exposing (defaultPieConfig)
import Task
import Triangle2d exposing (Triangle2d)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (fill, patternUnits, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core
import TypedSvg.Filters as Fe
import TypedSvg.Filters.Attributes as Fa
import TypedSvg.Types exposing (CoordinateSystem(..), Fill(..), InValue(..), Transform(..))
import Vector2d
import VoronoiDiagram2d exposing (VoronoiDiagram2d)


port saveScore : Value -> Cmd msg



-- Types


type Msg
    = Tick Float
    | Clicked CellState Float Float
    | ScreenSize Int Int
    | NewGame
    | HowToPlayClicked
    | StartLevel Level


type alias Model =
    { stage : Stage
    , screen : BoundingBox2d
    , levelsAchieved : Dict Int Float
    }


type Stage
    = Intro { frame : Int }
    | HowToPlay
    | Game GameModel
    | GameComplete { time : Float, currentLevel : Level }
    | GameLost Level
    | LevelSelection
    | Transition Float Stage Stage


type alias GameModel =
    { diagram : VoronoiDiagram2d Site
    , simulation : Force.State Int
    , areas : { nature : Float, human : Float, ocean : Float }
    , seed : Random.Seed
    , points : Int
    , time : Float
    , currentLevel : Level
    }


type alias Site =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , id : Int
    , state : CellState
    }


type CellState
    = Nature
    | Human
    | Ocean


type alias Level =
    { id : Int
    , seed : Random.Seed
    , goodTime : Int
    , badTime : Int
    , maxPoints : Int
    , pointEvaporationRate : Int
    , stateDistribution : List ( Float, CellState )
    , frenzy : Float
    , tolerance : Float
    , introText : String
    }



-- Constants


maxSpeed : Float
maxSpeed =
    3


levels : List Level
levels =
    [ { id = 1
      , seed = Random.initialSeed 432658723845932
      , goodTime = 30
      , badTime = 200
      , maxPoints = 5000
      , pointEvaporationRate = 0
      , stateDistribution = [ ( 0.5, Nature ), ( 0.5, Human ) ]
      , frenzy = 1
      , tolerance = 0.05
      , introText = "Fill up the meter by keeping the green and grey tiles equal in area"
      }
    , { id = 2
      , seed = Random.initialSeed 445364353454932
      , goodTime = 40
      , badTime = 220
      , maxPoints = 6000
      , pointEvaporationRate = 1
      , stateDistribution = [ ( 0.6, Nature ), ( 0.4, Human ) ]
      , frenzy = 1
      , tolerance = 0.02
      , introText = "You'll have to be more precise and faster this time"
      }
    , { id = 3
      , seed = Random.initialSeed 412356798765413
      , goodTime = 50
      , badTime = 260
      , maxPoints = 6000
      , pointEvaporationRate = 1
      , stateDistribution = [ ( 0.3, Nature ), ( 0.7, Human ) ]
      , frenzy = 3
      , tolerance = 0.01
      , introText = "The tiles are going crazy!"
      }
    , { id = 4
      , seed = Random.initialSeed 372589457398
      , goodTime = 50
      , badTime = 260
      , maxPoints = 6000
      , pointEvaporationRate = 1
      , stateDistribution = [ ( 0.4, Nature ), ( 0.59, Human ), ( 0.01, Ocean ) ]
      , frenzy = 2
      , tolerance = 0.02
      , introText = "Blue tiles work a bit differently!"
      }
    ]



-- Program


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { screen =
            BoundingBox2d.fromExtrema { minX = 0, maxX = 2300, minY = 0, maxY = 900 }
      , stage = Intro { frame = 0 }
      , levelsAchieved =
            D.decodeValue (D.keyValuePairs D.float) flags
                |> Result.withDefault []
                |> List.filterMap (\( k, v ) -> Maybe.map (\intKey -> ( intKey, v )) (String.toInt k))
                |> Dict.fromList
      }
        |> initGameModel
            { id = 4
            , seed = Random.initialSeed 372589457398
            , goodTime = 50
            , badTime = 260
            , maxPoints = 6000
            , pointEvaporationRate = 1
            , stateDistribution = [ ( 0.4, Nature ), ( 0.59, Human ), ( 0.01, Ocean ) ]
            , frenzy = 2
            , tolerance = 0.05
            , introText = "Blue tiles work a bit differently!"
            }
    , Task.perform (\{ viewport } -> ScreenSize (floor viewport.width) (floor viewport.height)) Browser.Dom.getViewport
    )


transitionTo : Stage -> Model -> Model
transitionTo stage model =
    { model | stage = Transition 1000 model.stage stage }


initGameModel : Level -> Model -> Model
initGameModel level model =
    let
        ( sites, seed ) =
            Random.step (sitesGenerator model.screen level.stateDistribution) level.seed

        diagram =
            buildDiagram sites
    in
    model
        |> transitionTo
            (Game
                { diagram = diagram
                , simulation = updateSimulation model.screen diagram
                , areas = { human = 0, nature = 0, ocean = 0 }
                , seed = seed
                , points = 0
                , time = 0
                , currentLevel = level
                }
            )


sitesGenerator : BoundingBox2d -> List ( Float, CellState ) -> Random.Generator (Array Site)
sitesGenerator screen distribution =
    Random.map (List.indexedMap (\index site -> { site | id = index }) >> Array.fromList)
        (Random.list 40
            (Random.map3 (\x y state -> { x = x, y = y, vx = 0, vy = 0, id = -1, state = state })
                (Random.float (BoundingBox2d.minX screen) (BoundingBox2d.maxX screen))
                (Random.float (BoundingBox2d.minY screen) (BoundingBox2d.maxY screen))
                (case distribution of
                    [] ->
                        Random.constant Human

                    head :: tail ->
                        Random.weighted head tail
                )
            )
        )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.stage ) of
        ( ScreenSize w h, c ) ->
            ( { model | screen = BoundingBox2d.fromExtrema { minX = 0, maxX = toFloat w, minY = 0, maxY = toFloat h } }
            , Cmd.none
            )

        ( _, Game submodel ) ->
            let
                newSubmodel =
                    updateGame model.screen msg submodel
            in
            if newSubmodel.points > submodel.currentLevel.maxPoints then
                let
                    scores =
                        Dict.insert submodel.currentLevel.id (min (Dict.get submodel.currentLevel.id model.levelsAchieved |> Maybe.withDefault newSubmodel.time) newSubmodel.time) model.levelsAchieved
                in
                ( { model | levelsAchieved = scores }
                    |> transitionTo (GameComplete { time = newSubmodel.time, currentLevel = submodel.currentLevel })
                , saveScore (Json.Encode.dict String.fromInt Json.Encode.float scores)
                )

            else if Array.length (VoronoiDiagram2d.vertices newSubmodel.diagram) <= 1 then
                ( model |> transitionTo (GameLost newSubmodel.currentLevel), Cmd.none )

            else
                ( { model | stage = Game newSubmodel }, Cmd.none )

        ( Tick _, Intro submodel ) ->
            ( { model | stage = Intro { submodel | frame = submodel.frame + 1 } }, Cmd.none )

        ( Tick t, Transition remaining before after ) ->
            let
                ( submodel, cmd ) =
                    update msg { model | stage = after }
            in
            if remaining - t <= 0 then
                ( submodel, cmd )

            else
                ( { model | stage = Transition (remaining - t) before submodel.stage }, cmd )

        ( _, Transition remaining before after ) ->
            let
                ( submodel, cmd ) =
                    update msg { model | stage = after }
            in
            ( { model | stage = Transition remaining before submodel.stage }, Cmd.none )

        ( NewGame, _ ) ->
            ( model |> transitionTo LevelSelection, Cmd.none )

        ( HowToPlayClicked, current ) ->
            ( transitionTo HowToPlay model, Cmd.none )

        ( StartLevel level, _ ) ->
            ( initGameModel level model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGame : BoundingBox2d -> Msg -> GameModel -> GameModel
updateGame screen msg model =
    case msg of
        Clicked state x y ->
            let
                maxId =
                    model.diagram
                        |> VoronoiDiagram2d.vertices
                        |> Array.foldl (\{ id } -> max id) -1

                diagram =
                    model.diagram
                        |> VoronoiDiagram2d.insertVertexBy siteToPoint { x = x, y = y, vx = 0, vy = 0, id = maxId + 1, state = flip model.currentLevel state }
                        |> Result.withDefault VoronoiDiagram2d.empty
            in
            { model | diagram = diagram, simulation = updateSimulation screen diagram }

        Tick delta ->
            let
                ( simulation, sites ) =
                    Force.tick model.simulation (model.diagram |> VoronoiDiagram2d.vertices |> Array.toList)

                newSites =
                    List.map2
                        (\newSite oldSite ->
                            let
                                v =
                                    Vector2d.fromComponents ( newSite.vx, newSite.vy )
                            in
                            if newSite.state == Ocean then
                                oldSite

                            else if Vector2d.length v > maxSpeed then
                                let
                                    newV =
                                        Vector2d.fromPolarComponents ( maxSpeed, Vector2d.polarComponents v |> Tuple.second )
                                in
                                { oldSite | x = oldSite.x + Vector2d.xComponent newV, y = oldSite.y + Vector2d.yComponent newV, vx = Vector2d.xComponent newV, vy = Vector2d.yComponent newV }

                            else
                                newSite
                        )
                        sites
                        (Array.toList (VoronoiDiagram2d.vertices model.diagram))

                newModel =
                    evaluateRules delta
                        screen
                        newSites
                        { model
                            | simulation = simulation
                            , time = model.time + delta
                        }

                areas =
                    updateAreas screen newModel.diagram
            in
            { newModel
                | areas = areas
                , points =
                    if aboutEqual model.currentLevel screen areas then
                        model.points + 10

                    else
                        max 0 (model.points - model.currentLevel.pointEvaporationRate)
            }

        _ ->
            model


updateSimulation : BoundingBox2d -> VoronoiDiagram2d Site -> Force.State Int
updateSimulation screen diagram =
    let
        center =
            Force.center (BoundingBox2d.midX screen) (BoundingBox2d.midY screen)

        manyBody =
            diagram
                |> VoronoiDiagram2d.vertices
                |> Array.toList
                |> List.map .id
                |> Force.manyBodyStrength -20

        links =
            VoronoiDiagram2d.toDelaunayTriangulation diagram
                |> computeEdges
                |> Force.customLinks 1
    in
    Force.simulation
        [ links
        , manyBody
        , center
        ]
        |> Force.iterations 1000


computeEdges : DelaunayTriangulation2d Site -> List { source : Int, target : Int, distance : Float, strength : Maybe Float }
computeEdges delaunay =
    List.foldl
        (\{ vertices } dict ->
            let
                ( a, b, c ) =
                    vertices
            in
            dict
                |> insertElligibleTile a b
                |> insertElligibleTile b c
                |> insertElligibleTile a c
        )
        Dict.empty
        (DelaunayTriangulation2d.faces delaunay)
        |> Dict.values


insertElligibleTile : Site -> Site -> Dict ( Int, Int ) { source : Int, target : Int, distance : Float, strength : Maybe Float } -> Dict ( Int, Int ) { source : Int, target : Int, distance : Float, strength : Maybe Float }
insertElligibleTile a b dict =
    if a.state == Ocean || b.state == Ocean then
        dict

    else
        Dict.insert ( min a.id b.id, max a.id b.id ) (computeEdge a b) dict


computeEdge : Site -> Site -> { source : Int, target : Int, distance : Float, strength : Maybe Float }
computeEdge a b =
    let
        distance =
            sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)
    in
    { source = a.id
    , target = b.id
    , distance =
        max 30
            (if a.state == b.state then
                distance * 1

             else
                distance * 0.9
            )
    , strength = Just 1
    }


updateAreas : BoundingBox2d -> VoronoiDiagram2d Site -> { nature : Float, human : Float, ocean : Float }
updateAreas screen diagram =
    diagram
        |> VoronoiDiagram2d.polygons screen
        |> List.foldl
            (\( { state }, poly ) totals ->
                case state of
                    Nature ->
                        { totals | nature = totals.nature + Polygon2d.area poly }

                    Human ->
                        { totals | human = totals.human + Polygon2d.area poly }

                    Ocean ->
                        { totals | ocean = totals.ocean + Polygon2d.area poly }
            )
            { human = 0, nature = 0, ocean = 0 }


aboutEqual : Level -> BoundingBox2d -> { nature : Float, human : Float, ocean : Float } -> Bool
aboutEqual level screen { nature, human, ocean } =
    let
        ( w, h ) =
            BoundingBox2d.dimensions screen

        scores =
            List.map
                (\( _, cell ) ->
                    case cell of
                        Nature ->
                            nature

                        Human ->
                            human

                        Ocean ->
                            ocean
                )
                level.stateDistribution

        tolerance =
            w * h * level.tolerance

        aboutEqualHelp list =
            case list of
                [] ->
                    False

                [ x ] ->
                    True

                x :: y :: xs ->
                    abs (x - y) < tolerance && aboutEqualHelp (y :: xs)
    in
    aboutEqualHelp scores



-- Rules


type alias Rule =
    ( Site -> List ( Site, Polygon2d ) -> Maybe Float
    , Site -> Random.Generator (List Site)
    )


evaluateRules : Float -> BoundingBox2d -> List Site -> GameModel -> GameModel
evaluateRules delta screen sites model =
    let
        ( ( newSites, needsSimulationUpdate ), seed ) =
            Random.step (rules delta screen sites model) model.seed

        diagram =
            buildDiagram newSites
    in
    { model
        | diagram = diagram
        , seed = seed
        , simulation =
            if needsSimulationUpdate then
                updateSimulation screen diagram

            else
                model.simulation
    }


rules : Float -> BoundingBox2d -> List Site -> GameModel -> Random.Generator ( Array Site, Bool )
rules delta screen sites model =
    ruleEngine delta
        screen
        sites
        model.diagram
        [ outOfBoundsEliminationRule screen

        -- , jiggleRule model.currentLevel.frenzy
        , subsumptionRule
        , isolationRule
        ]


ruleEngine : Float -> BoundingBox2d -> List Site -> VoronoiDiagram2d Site -> List Rule -> Random.Generator ( Array Site, Bool )
ruleEngine delta screen sites diagram ruleset =
    let
        normalized =
            delta / 1000

        addToSet a b =
            Maybe.withDefault Set.empty
                >> Set.insert a
                >> Set.insert b
                >> Just

        neigborhood =
            diagram
                |> VoronoiDiagram2d.polygons screen
                |> List.map (\( site, poly ) -> ( site.id, ( site, poly ) ))
                |> Dict.fromList

        neigbors =
            List.foldl
                (\{ vertices } dict ->
                    let
                        ( a, b, c ) =
                            vertices
                    in
                    dict
                        |> Dict.update a.id (addToSet b.id c.id)
                        |> Dict.update b.id (addToSet a.id c.id)
                        |> Dict.update c.id (addToSet a.id b.id)
                )
                Dict.empty
                (DelaunayTriangulation2d.faces (VoronoiDiagram2d.toDelaunayTriangulation diagram))
                |> Dict.map (\k set -> Set.toList set |> List.filterMap (\id -> Dict.get id neigborhood))

        selectRule site =
            let
                applicableRules =
                    List.filterMap
                        (\( probFn, evalFn ) ->
                            let
                                currentNeigbors =
                                    Dict.get site.id neigbors |> Maybe.withDefault []
                            in
                            Maybe.map (\p -> ( p * normalized, evalFn site )) (probFn site currentNeigbors)
                        )
                        ruleset
            in
            Random.weighted ( 1 - List.sum (List.map Tuple.first applicableRules), Random.constant [ site ] )
                applicableRules
                |> Random.andThen identity
    in
    sites
        |> List.foldr
            (\site gen ->
                Random.map2
                    (\h t ->
                        case ( h, t ) of
                            ( [ x ], ( xs, mod ) ) ->
                                ( x :: xs, mod )

                            ( [], ( xs, _ ) ) ->
                                ( xs, True )

                            ( more, ( xs, _ ) ) ->
                                ( more ++ xs, True )
                    )
                    (selectRule site)
                    gen
            )
            (Random.constant ( [], False ))
        |> Random.map (Tuple.mapFirst Array.fromList)


withProbabilites : ({ nature : Maybe Float, ocean : Maybe Float, human : Maybe Float } -> { nature : Maybe Float, ocean : Maybe Float, human : Maybe Float }) -> (Site -> Random.Generator (List Site)) -> Rule
withProbabilites probs fn =
    let
        p =
            probs { nature = Nothing, ocean = Nothing, human = Nothing }
    in
    ( \site _ ->
        case site.state of
            Nature ->
                p.nature

            Ocean ->
                p.ocean

            Human ->
                p.human
    , fn
    )


eliminate : a -> Random.Generator (List b)
eliminate _ =
    Random.constant []


outOfBoundsEliminationRule : BoundingBox2d -> Rule
outOfBoundsEliminationRule screen =
    ( \site _ ->
        if BoundingBox2d.contains (Point2d.fromCoordinates ( site.x, site.y )) screen then
            Nothing

        else
            Just 0.1
    , eliminate
    )


jiggleRule : Float -> Rule
jiggleRule frenzy =
    withProbabilites (\r -> { r | nature = Just (0.2 * frenzy) })
        (\site ->
            Random.map4 (\x0 x1 y0 y1 -> [ { site | vx = x0 + x1, vy = y0 + y1 } ])
                (Random.float -5 5)
                (Random.float -5 5)
                (Random.float -5 5)
                (Random.float -5 5)
        )


subsumptionRule : Rule
subsumptionRule =
    ( \site neibors ->
        if List.all (\( n, _ ) -> n.state == site.state) neibors then
            Just 0.03

        else if List.all (\( n, _ ) -> n.state == site.state || n.state == Ocean) neibors then
            Just 0.01

        else
            Nothing
    , eliminate
    )


isolationRule : Rule
isolationRule =
    ( \site neibors ->
        let
            fst =
                List.head neibors |> Maybe.map (Tuple.first >> .state) |> Maybe.withDefault site.state
        in
        if site.state /= Ocean && List.all (\( n, _ ) -> n.state /= site.state && n.state == fst) neibors then
            Just (3 * (List.foldl (\( _, poly ) total -> total + Polygon2d.area poly) 0 neibors ^ 2 / 1000000 ^ 2))

        else
            Nothing
    , eliminate
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize ScreenSize
        ]



-- Utilities


flip : Level -> CellState -> CellState
flip level cell =
    case cell of
        Nature ->
            Human

        Human ->
            Nature

        Ocean ->
            Ocean


siteToPoint : Site -> Point2d
siteToPoint site =
    Point2d.fromCoordinates ( site.x, site.y )



-- View


view : Model -> Html Msg
view model =
    Html.main_ [ Html.Attributes.style "overflow" "hidden" ] (globalStyle :: viewStage model.levelsAchieved model.screen model.stage)


viewStage : Dict Int Float -> BoundingBox2d -> Stage -> List (Html Msg)
viewStage levelsAchieved screen stage =
    case stage of
        Intro { frame } ->
            [ renderDiagram screen (buildDiagram (sitesFromLogo screen frame)) []
            , if frame > 200 then
                Html.button [ Html.Events.onClick NewGame, style "position" "absolute", style "top" "60vh" ] [ Html.text "Play" ]

              else
                Html.text ""

            -- , if frame > 220 then
            --     Html.button [ Html.Events.onClick HowToPlayClicked, style "position" "absolute", style "top" "75vh" ] [ Html.text "How to Play" ]
            --
            --   else
            --     Html.text ""
            ]

        Game gameModel ->
            [ renderDiagram screen gameModel.diagram [ scoreBoardView screen gameModel ]
            , Html.p [ Html.Attributes.class "hint" ] [ Html.text gameModel.currentLevel.introText ]
            ]

        GameComplete { time, currentLevel } ->
            [ renderDiagram screen (buildDiagram (screenFrame screen)) []
            , Html.section []
                [ Html.h2 [] [ Html.text "Well Done!" ]
                , Html.h1 [] [ Html.text (String.fromInt (floor (time / 1000))) ]
                , Html.p [] [ Html.text "You have completed the level in ", Html.text (String.fromInt (floor (time / 1000))), Html.text " seconds." ]
                , Html.p [ Html.Attributes.class "star-popin" ] [ starRating currentLevel (Just time) ]
                , Html.button [ Html.Events.onClick NewGame ] [ Html.text "Continue" ]
                ]
            ]

        GameLost currentLevel ->
            [ renderDiagram screen (buildDiagram (screenFrame screen)) []
            , Html.section []
                [ Html.h2 [] [ Html.text "Oh oh!" ]
                , Html.p [] [ Html.text "Unfortunately the balance in the ecosystem was lost and everything died." ]
                , Html.p [] [ Html.button [ Html.Events.onClick (StartLevel currentLevel) ] [ Html.text "Try Again" ] ]
                , Html.p [] [ Html.button [ Html.Events.onClick NewGame ] [ Html.text "Level Selection" ] ]
                ]
            ]

        HowToPlay ->
            [ renderDiagram screen (buildDiagram (screenFrame screen)) []
            , Html.section []
                [ Html.p []
                    [ Html.text "The goal of the game is to fill up your point meter. The meter will start filling up when a perfect balance between nature and technology is achieved. You can do this by making sure the area taken by green and dark cells is the same. Simply click on a cell to split it into another cell with the oposite kind. When a balance is achieved the meter will glow and you will start filling up your point meter."
                    ]
                , Html.button [ Html.Events.onClick NewGame ] [ Html.text "Start Game" ]
                ]
            ]

        LevelSelection ->
            let
                nextOpenLevel =
                    1 + (Dict.keys levelsAchieved |> List.maximum |> Maybe.withDefault 0)
            in
            [ renderDiagram screen (buildDiagram (screenFrame screen)) []
            , Html.section []
                [ Html.h2 [] [ Html.text "Select level" ]
                , levels
                    |> List.filter (\level -> level.id <= nextOpenLevel)
                    |> List.map (\level -> viewLevelSelection (Dict.get level.id levelsAchieved) level)
                    |> Html.div [ Html.Attributes.class "level-selection" ]
                ]
            ]

        Transition _ before after ->
            [ Html.div [ Html.Attributes.class "transition" ]
                [ Html.div [ Html.Attributes.class "before" ] (viewStage levelsAchieved screen before)
                , Html.div [ Html.Attributes.class "after" ] (viewStage levelsAchieved screen after)
                ]
            ]


viewLevelSelection : Maybe Float -> Level -> Html Msg
viewLevelSelection bestTime level =
    Html.button [ Html.Events.onClick (StartLevel level), Html.Attributes.class "level-button" ]
        [ Html.h3 [] [ Html.text (String.fromInt level.id) ]
        , starRating level bestTime
        , Html.span []
            [ Html.text (Maybe.withDefault "-" (Maybe.map (\t -> String.fromInt (floor (t / 1000)) ++ "s") bestTime)) ]
        ]


starRating : Level -> Maybe Float -> Html msg
starRating level potentialBestTime =
    case potentialBestTime of
        Just bestTime ->
            if bestTime <= 1000 * toFloat level.goodTime then
                Html.span []
                    [ Html.b [] [ Html.text "★★★" ] ]

            else if bestTime <= 1000 * toFloat level.badTime then
                Html.span []
                    [ Html.b [] [ Html.text "★★" ]
                    , Html.i [] [ Html.text "☆" ]
                    ]

            else
                Html.span []
                    [ Html.b [] [ Html.text "★" ]
                    , Html.i [] [ Html.text "☆☆" ]
                    ]

        Nothing ->
            Html.span []
                [ Html.i [] [ Html.text "☆☆☆" ]
                ]


screenElement : BoundingBox2d -> List (Attribute msg) -> List (Html msg) -> Html msg
screenElement bbox attrs =
    let
        ( w, h ) =
            BoundingBox2d.dimensions bbox
    in
    svg [ width w, height h, viewBox (BoundingBox2d.minX bbox) (BoundingBox2d.minY bbox) w h ]


equilateralTriangle : Point2d -> Float -> Triangle2d
equilateralTriangle center radius =
    let
        up =
            Vector2d.fromPolarComponents ( radius, -pi / 2 )

        left =
            Vector2d.rotateBy (2 * pi / 3) up

        right =
            Vector2d.rotateBy (2 * pi / 3) left
    in
    Triangle2d.fromVertices
        ( center |> Point2d.translateBy up
        , center |> Point2d.translateBy left
        , center |> Point2d.translateBy right
        )


globalStyle : Html msg
globalStyle =
    Html.node "style"
        []
        [ Html.text """
            body {
                padding: 0;
                margin: 0;
                overflow: hidden;
            }
            main {
                display: flex;
                width: 100vw;
                height: 100vh;
                justify-content: center;
            }
            .transition .before {
                animation: 1s animate-out;
                z-index: 13;
                position: absolute;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                justify-content: center;
                display: flex;
            }
            .transition .after {
                display: flex;
                opacity: 1;
                position: absolute;
                top: 0;
                left: 0;
                right: 0;
                bottom: 0;
                justify-content: center;
            }
            @keyframes animate-out {
                0% {
                    transform: scale(1);
                    opacity: 1;
                }
                100% {
                    transform: scale(1.8);
                    opacity: 0;
                }
            }
            .hint {
                animation: 10s hint;
                animation-delay: 1s;
                transform: translate(110vw,0);
                z-index: 24;
                opacity: 0.1;
                position: absolute;
                bottom: 5vh;
                left: 50px;
                font: 3.6vh Helvetica, sans-serif;
                color: white;
                text-shadow: 1px 1px 1px rgba(30%,30%,30%,30%);
                pointer-events: none;
            }
            @keyframes hint {
                10% {
                    transform: translate(0,0);
                    opacity: 1;
                }
                90% {
                    transform: translate(0,0);
                    opacity: 1;
                }
                100% {
                    transform: translate(110vw,0);
                    opacity: 0;
                }
            }
            svg {
                position: absolute;
                top:0;
            }
            section {
                margin: auto;
                text-align: center;
                font-family: Helvetica, sans-serif;
                top: 10vh;
                z-index: 10;
                position: absolute;
                color: white;
                text-shadow: 1px 1px 1px rgba(30%,30%,30%,1);
                width: 80vw;
                max-width: 850px;
            }
            .level-selection {
                display: flex;
                flex-direction: row;
            }
            .level-button {
                margin: 20px; display: flex; flex-direction: column;
                border: 5px solid rgba(10%,75%,10%,1);
                border-radius: 10px;
                box-sizing: border-box;
                padding: 0;
                align-items: stretch;
                width: 20vh;
            }
            .level-button:hover {
                border-color: rgba(86%,81%,9%,1);
            }
            .level-button h3 {
                font-size: 3vh;
            }
            .level-button span {
                background: rgba(10%,75%,10%,1);
                color: rgba(30%,30%,30%,1);

            }
            .level-button:hover span {
                background: rgba(86%,81%,9%,1);
                color: rgba(30%,30%,30%,1);
            }
            .star-popin {
                font-size: 6.4vh;
            }
            h1 {
                font-size: 9vh;
            }
            h2 {
                font-size: 4.5vh;
            }
            p {
                font-size: 3.2vh;
            }
            button {
                margin: auto;
                z-index: 10;
                font-size: 5vh;
                border: none;
                background: transparent;
                color: rgba(10%,75%,10%,1);
                text-align: center;
                font-weight: bold;
                text-shadow: 1px 1px 1px rgba(30%,30%,30%,1);
            }

            button:hover {
                color: rgba(86%,81%,9%,1);
            }
            b {
                color: rgba(86%,81%,9%,1);
                font-weight: normal;
            }
            i {
                font-style: normal;
            }

        """
        ]


scoreBoardView : BoundingBox2d -> GameModel -> Html msg
scoreBoardView screen { areas, points, time, currentLevel } =
    let
        innerRadius =
            35

        pointHeight =
            (toFloat points / toFloat currentLevel.maxPoints) * innerRadius * 2
    in
    TypedSvg.circle [ cx 0, cy 0, r 52, fill (Fill (Color.rgb 0 0 0)) ] []
        :: TypedSvg.clipPath [ TypedSvg.Core.attribute "id" "score-clip" ]
            [ TypedSvg.rect [ x -innerRadius, y (innerRadius - pointHeight), width (innerRadius * 2), height pointHeight ] []
            ]
        :: (Shape.pie
                { defaultPieConfig | innerRadius = 40, outerRadius = 50 }
                [ areas.nature, areas.human, areas.ocean ]
                |> List.map2
                    (\state arc ->
                        Path.element (Shape.arc arc)
                            ([ fill (Fill (stateToColor state)) ]
                                ++ (if aboutEqual currentLevel screen areas then
                                        [ TypedSvg.Core.attribute "filter" "url(#glow)" ]

                                    else
                                        []
                                   )
                            )
                    )
                    [ Nature, Human, Ocean ]
           )
        ++ [ TypedSvg.circle [ cx 0, cy 0, r innerRadius, fill (Fill (Color.rgb 0.8 0.2 0.2)), TypedSvg.Core.attribute "clip-path" "url(#score-clip)" ] []
           , TypedSvg.text_
                [ x 0
                , y 0
                , TypedSvg.Core.attribute "dy" "0.37em"
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
                , fill (Fill (Color.rgb 1 1 1))
                , TypedSvg.Attributes.InPx.fontSize 28
                , TypedSvg.Attributes.fontFamily [ "Helvetica", "sans-serif" ]
                ]
                [ TypedSvg.Core.text (String.fromInt (floor (time / 1000))) ]
           ]
        |> TypedSvg.g [ TypedSvg.Attributes.transform [ Translate 60 60 ] ]


screenFrame screen =
    let
        border =
            50

        wiggle =
            2

        ( width, height ) =
            BoundingBox2d.dimensions screen
    in
    -- top left
    [ ( border / 2, border / 2 - wiggle, Nature )
    , ( border / 2 + wiggle, 3 * border / 2, Nature )
    , ( 3 * border / 2, border / 2 + wiggle, Nature )
    , ( 3 * border / 2, 3 * border / 2, Human )

    -- top right
    , ( width - border / 2 + wiggle, border / 2, Nature )
    , ( width - border / 2, 3 * border / 2, Nature )
    , ( width - 3 * border / 2, border / 2, Nature )
    , ( width - 3 * border / 2, 3 * border / 2, Human )

    -- bottom right
    , ( width - border / 2, height - border / 2, Nature )
    , ( width - border / 2, height - 3 * border / 2, Nature )
    , ( width - 3 * border / 2, height - border / 2, Nature )
    , ( width - 3 * border / 2 - wiggle, height - 3 * border / 2 - wiggle, Human )

    -- bottom left
    , ( border / 2, height - border / 2, Nature )
    , ( border / 2, height - 3 * border / 2 - wiggle, Nature )
    , ( 3 * border / 2, height - border / 2, Nature )
    , ( 3 * border / 2 - wiggle, height - 3 * border / 2, Human )

    -- center
    , ( BoundingBox2d.midX screen, BoundingBox2d.midY screen, Human )
    ]
        |> List.indexedMap
            (\index ( x, y, state ) ->
                { x = x, y = y, vx = 0, vy = 0, id = index, state = state }
            )
        |> Array.fromList


sitesFromLogo screen frame =
    let
        speed =
            1

        scroll =
            3

        factor =
            Tuple.first (BoundingBox2d.dimensions screen) / 2240

        yOffset =
            max 50 (BoundingBox2d.midY screen - 300 * factor - toFloat (max 0 (frame * scroll - List.length logo * scroll * speed)))
    in
    logo
        |> List.take (frame // speed)
        |> List.indexedMap
            (\index ( x, y, state ) ->
                { x = x * factor, y = y * factor + yOffset, vx = 0, vy = 0, id = index, state = state }
            )
        |> Array.fromList


buildDiagram : Array Site -> VoronoiDiagram2d Site
buildDiagram sites =
    VoronoiDiagram2d.fromVerticesBy siteToPoint sites
        |> Result.withDefault VoronoiDiagram2d.empty


renderDiagram : BoundingBox2d -> VoronoiDiagram2d Site -> List (Html Msg) -> Html Msg
renderDiagram clip diagram additional =
    [ TypedSvg.defs []
        [ TypedSvg.pattern [ TypedSvg.Core.attribute "id" "diamonds", viewBox 0 0 16 20, width 16, height 20, patternUnits CoordinateSystemUserSpaceOnUse ]
            [ TypedSvg.rect [ width 16, height 20, fill (Fill (Color.rgb 0.3 0.3 0.3)) ] []
            , TypedSvg.path [ TypedSvg.Attributes.d "M8 0v20L0 10M16 0v10L8 0M16 10v10H8", fill (Fill (Color.rgba 0.86 0.81 0.09 0.03)) ] []
            ]
        , TypedSvg.pattern [ TypedSvg.Core.attribute "id" "sea-floor", viewBox 0 0 30 30, width 30, height 30, patternUnits CoordinateSystemUserSpaceOnUse ]
            [ TypedSvg.rect [ width 31, height 31, fill (Fill (Color.rgb 0.1 0.1 0.8)) ] []
            , TypedSvg.path [ TypedSvg.Attributes.d "M15 3L19 11L27 15L19 19L15 27L11 19L3 15L11 11Z", stroke (Color.rgba 1 1 1 0.2), fill (Fill (Color.rgba 0.5 0.5 0.5 0.2)) ] []
            , TypedSvg.path [ TypedSvg.Attributes.d "M0 0L30 30M0 30L30 0L5 25L30 30L25 5L0 0L5 25M0 30L5 5L30 0L 25 25L0 30M0 13", stroke (Color.rgba 1 1 1 0.2), fill FillNone ] []
            ]
        , TypedSvg.filter [ TypedSvg.Core.attribute "id" "glow", TypedSvg.Attributes.width (TypedSvg.Types.percent 300), TypedSvg.Attributes.height (TypedSvg.Types.percent 300) ]
            [ Fe.morphology [ Fa.morphologyOperator TypedSvg.Types.MorphologyOperatorDilate, Fa.radius 3 3, Fa.in_ InSourceGraphic, Fa.result "thicken" ]
                []
            , Fe.gaussianBlur [ Fa.in_ (InReference "thicken"), TypedSvg.Attributes.stdDeviation "5", Fa.result "blurred" ]
                [ TypedSvg.animate [ TypedSvg.Attributes.attributeName "stdDeviation", TypedSvg.Attributes.animationValues [ 3, 5, 3 ], TypedSvg.Attributes.dur (TypedSvg.Types.Duration "1s"), TypedSvg.Attributes.repeatCount TypedSvg.Types.RepeatIndefinite ] []
                ]
            , Fe.merge []
                [ Fe.mergeNode [ Fa.in_ (InReference "blurred") ] []
                , Fe.mergeNode [ Fa.in_ InSourceGraphic ] []
                ]
            ]
        ]
    , VoronoiDiagram2d.polygons clip diagram
        |> List.sortBy
            (\( { state }, _ ) ->
                case state of
                    Nature ->
                        2

                    Human ->
                        1

                    Ocean ->
                        0
            )
        |> List.concatMap
            (\( { state, x, y }, poly ) ->
                case state of
                    Ocean ->
                        [ Geometry.Svg.polygon2d
                            [ fill (Fill (stateToColor state))
                            , stroke (Color.rgb 0.86 0.81 0.09)
                            , strokeWidth 2
                            , TypedSvg.Core.attribute "fill" "url(#sea-floor)"
                            , custom "click"
                                (D.map
                                    (\( clx, cly ) ->
                                        { message = Clicked state clx cly
                                        , stopPropagation = True
                                        , preventDefault = True
                                        }
                                    )
                                    decodeMousePosition
                                )
                            ]
                            poly
                        ]

                    Human ->
                        [ Geometry.Svg.polygon2d
                            [ --fill (Fill (stateToColor state))
                              TypedSvg.Core.attribute "fill" "url(#diamonds)"
                            , stroke (Color.rgb 0.86 0.81 0.09)
                            , strokeWidth 2
                            , custom "click"
                                (D.map
                                    (\( clx, cly ) ->
                                        { message = Clicked state clx cly
                                        , stopPropagation = True
                                        , preventDefault = True
                                        }
                                    )
                                    decodeMousePosition
                                )
                            ]
                            poly
                        , Geometry.Svg.triangle2d
                            [ fill (Fill (Color.rgb 0.86 0.81 0.09))

                            -- , strokeWidth 2
                            -- , TypedSvg.Core.attribute "fill" "url(#diamonds)"
                            ]
                            (equilateralTriangle (Point2d.fromCoordinates ( x, y )) 6)

                        -- , circle [ fill (Fill (Color.rgb 0.8 0.8 0.8)), cx x, cy y, r 3 ] []
                        ]

                    Nature ->
                        [ Geometry.Svg.polygon2d
                            [ fill (Fill (Color.rgb 0.1 0.75 0.1))
                            , stroke (Color.rgb 0.1 0.75 0.1)
                            , strokeWidth 15
                            , TypedSvg.Core.attribute "stroke-location" "outside"
                            , TypedSvg.Core.attribute "stroke-linejoin" "round"
                            ]
                            poly
                        , Polygon2d.outerLoop poly
                            |> List.map Point2d.coordinates
                            |> computeCurve
                            |> (\path ->
                                    Path.element path
                                        [ fill (Fill (stateToColor state))
                                        , stroke (Color.rgb 0.1 0.75 0.1)
                                        , strokeWidth 15
                                        , TypedSvg.Core.attribute "stroke-location" "outside"
                                        , TypedSvg.Core.attribute "stroke-linejoin" "round"
                                        , custom "click"
                                            (D.map
                                                (\( clx, cly ) ->
                                                    { message = Clicked state clx cly
                                                    , stopPropagation = True
                                                    , preventDefault = True
                                                    }
                                                )
                                                decodeMousePosition
                                            )
                                        ]
                               )
                        , TypedSvg.circle [ cx x, cy y, r 8, fill (Fill (stateToColor state)), stroke (Color.rgb 0.1 0.75 0.1), strokeWidth 5 ] []
                        ]
            )
        |> g []
    ]
        ++ additional
        |> screenElement clip []


computeCurve points =
    case List.reverse points of
        [] ->
            []

        last :: _ ->
            List.foldl
                (\( x1, y1 ) ( ( x0, y0 ), newPoints ) ->
                    ( ( x1, y1 ), ( x1, y1 ) :: ( (x0 + x1 * 2) / 3, (y0 + y1 * 2) / 3 ) :: ( (x0 * 2 + x1) / 3, (y0 * 2 + y1) / 3 ) :: newPoints )
                )
                ( last, [] )
                points
                |> Tuple.second
                |> List.reverse
                |> Shape.basisCurveClosed
                |> List.singleton


stateToColor : CellState -> Color
stateToColor state =
    case state of
        Nature ->
            Color.rgb 0.1 0.8 0.1

        Human ->
            Color.rgb 0.5 0.5 0.5

        Ocean ->
            Color.rgb 0.1 0.1 0.8


decodeMousePosition : Decoder ( Float, Float )
decodeMousePosition =
    D.map2 Tuple.pair
        (D.oneOf [ D.field "clientX" D.float, D.field "offsetX" D.float ])
        (D.oneOf [ D.field "clientY" D.float, D.field "offsetY" D.float ])


logo =
    [ ( 150, 120, Nature ), ( 150, 90, Human ), ( 300, 120, Human ), ( 150, 150, Human ), ( 150, 200, Nature ), ( 150, 170, Human ), ( 300, 200, Human ), ( 150, 230, Human ), ( 150, 280, Nature ), ( 150, 250, Human ), ( 300, 280, Human ), ( 150, 310, Human ), ( 92, 158, Nature ), ( 59, 159, Human ), ( 73, 90, Human ), ( 103, 240, Nature ), ( 57, 235, Human ), ( 75, 314, Human ), ( 333, 118, Nature ), ( 333, 87, Human ), ( 501, 116, Human ), ( 480, 113, Nature ), ( 481, 79, Human ), ( 320, 198, Nature ), ( 334, 277, Nature ), ( 333, 304, Human ), ( 408, 204, Human ), ( 477, 280, Nature ), ( 477, 301, Human ), ( 497, 276, Human ), ( 473, 202, Nature ), ( 496, 201, Human ), ( 504, 301, Nature ), ( 525, 293, Human ), ( 495, 332, Human ), ( 572, 113, Nature ), ( 569, 88, Human ), ( 546, 112, Human ), ( 596, 110, Human ), ( 578, 221, Nature ), ( 552, 219, Human ), ( 606, 217, Human ), ( 583, 299, Nature ), ( 581, 317, Human ), ( 727, 295, Human ), ( 711, 295, Nature ), ( 709, 314, Human ), ( 720, 104, Nature ), ( 719, 81, Human ), ( 738, 105, Human ), ( 700, 102, Human ), ( 811, 99, Nature ), ( 810, 77, Human ), ( 793, 99, Human ), ( 829, 99, Human ), ( 810, 300, Nature ), ( 792, 300, Human ), ( 830, 298, Human ), ( 811, 318, Human ), ( 894, 97, Nature ), ( 875, 97, Human ), ( 915, 96, Human ), ( 894, 73, Human ), ( 897, 308, Nature ), ( 877, 308, Human ), ( 898, 322, Human ), ( 1016, 309, Human ), ( 1000, 308, Nature ), ( 1000, 323, Human ), ( 1000, 280, Human ), ( 1061, 99, Nature ), ( 1060, 77, Human ), ( 1043, 99, Human ), ( 1079, 99, Human ), ( 1061, 200, Nature ), ( 1043, 200, Human ), ( 1079, 200, Human ), ( 1060, 300, Nature ), ( 1042, 300, Human ), ( 1080, 298, Human ), ( 1061, 318, Human ), ( 1230, 105, Nature ), ( 1230, 70, Human ), ( 1230, 140, Human ), ( 1230, 199, Nature ), ( 1230, 163, Human ), ( 1230, 234, Human ), ( 1230, 292, Nature ), ( 1230, 257, Human ), ( 1230, 327, Human ), ( 1172, 149, Nature ), ( 1139, 151, Human ), ( 1153, 70, Human ), ( 1183, 245, Nature ), ( 1137, 239, Human ), ( 1155, 332, Human ), ( 1299, 153, Nature ), ( 1296, 252, Nature ), ( 1306, 297, Human ), ( 1337, 251, Human ), ( 1317, 189, Human ), ( 1331, 141, Human ), ( 1297, 83, Human ), ( 1430, 109, Nature ), ( 1430, 73, Human ), ( 1430, 145, Human ), ( 1430, 205, Nature ), ( 1430, 169, Human ), ( 1430, 241, Human ), ( 1430, 265, Human ), ( 1430, 337, Human ), ( 1372, 154, Nature ), ( 1383, 253, Nature ), ( 1499, 158, Nature ), ( 1496, 260, Nature ), ( 1516, 341, Human ), ( 1537, 259, Human ), ( 1517, 195, Human ), ( 1531, 146, Human ), ( 1497, 86, Human ), ( 1591, 99, Nature ), ( 1590, 77, Human ), ( 1573, 99, Human ), ( 1609, 99, Human ), ( 1591, 200, Nature ), ( 1573, 200, Human ), ( 1609, 200, Human ), ( 1590, 300, Nature ), ( 1572, 300, Human ), ( 1610, 298, Human ), ( 1591, 318, Human ), ( 1672, 113, Nature ), ( 1669, 88, Human ), ( 1646, 112, Human ), ( 1696, 110, Human ), ( 1678, 221, Nature ), ( 1652, 219, Human ), ( 1706, 217, Human ), ( 1683, 299, Nature ), ( 1681, 317, Human ), ( 1827, 295, Human ), ( 1811, 295, Nature ), ( 1809, 314, Human ), ( 1820, 104, Nature ), ( 1819, 81, Human ), ( 1838, 105, Human ), ( 1800, 102, Human ), ( 1920, 100, Nature ), ( 1890, 100, Human ), ( 1917, 82, Human ), ( 1909, 200, Nature ), ( 1896, 200, Human ), ( 1910, 300, Nature ), ( 1893, 300, Human ), ( 1910, 314, Human ), ( 1926, 300, Human ), ( 1923, 200, Human ), ( 1939, 110, Human ), ( 1956, 104, Nature ), ( 1954, 82, Human ), ( 1972, 104, Human ), ( 1986, 176, Nature ), ( 1971, 182, Human ), ( 2003, 182, Human ), ( 2021, 100, Nature ), ( 2021, 83, Human ), ( 2004, 91, Human ), ( 2031, 114, Human ), ( 2060, 95, Nature ), ( 2059, 75, Human ), ( 2090, 96, Human ), ( 2066, 155, Nature ), ( 2050, 155, Human ), ( 2090, 155, Human ), ( 2070, 297, Nature ), ( 2090, 297, Human ), ( 2050, 297, Human ), ( 2072, 314, Human ) ]
        |> List.sortBy (\( x, y, s ) -> modBy 13 (round x) - modBy 34 (round y))
