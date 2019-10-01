module Main exposing (main)

import Array exposing (Array)
import Browser
import Cell
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Random
import Random.Array
import Svg exposing (Svg)
import Svg.Attributes
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type CellShape
    = Square
    | Squircle
    | Circle


type alias CellLook =
    { size : Int
    , shape : CellShape
    }


type alias ColorScheme =
    { alive : String
    , dead : String
    , background : String
    }


type alias Model =
    { cells : Cell.Cells
    , generation : Int
    , refreshInterval : Float
    , isAutomated : Bool
    , cellLook : CellLook
    , density : Int
    , margin : Int
    , worldLength : Cell.Coordinates
    , colorScheme : ColorScheme
    , rule : Cell.Rule
    }


cellShapeToString : CellShape -> String
cellShapeToString shape =
    case shape of
        Square ->
            "Square"

        Squircle ->
            "Squircle"

        Circle ->
            "Circle"


cellShapeFromString : String -> CellShape
cellShapeFromString shape =
    if shape == "Square" then
        Square

    else if shape == "Squircle" then
        Squircle

    else if shape == "Circle" then
        Circle

    else
        Square


init : () -> ( Model, Cmd Msg )
init _ =
    let
        cell =
            False

        updateInterval =
            100

        cellLook =
            { size = 5
            , shape = Square
            }

        density =
            30

        margin =
            10

        worldLength =
            { x = 100
            , y = 100
            }

        colorScheme =
            { alive = "#a9d8f5"
            , dead = "#221c30"
            , background = "#0e190e"
            }

        rule =
            Cell.Life
    in
    ( { cells = Array.repeat worldLength.y <| Array.repeat worldLength.x cell
      , generation = 0
      , refreshInterval = updateInterval
      , isAutomated = False
      , cellLook = cellLook
      , density = density
      , margin = margin
      , worldLength = worldLength
      , colorScheme = colorScheme
      , rule = rule
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GenerateRandom
    | Reset Cell.Cells
    | Tick
    | Start
    | Stop
    | UpdateRefreshInterval String
    | UpdateCellSize String
    | UpdateCellShape String
    | UpdateDensity String
    | UpdateMargin String
    | UpdateWorldXLength String
    | UpdateWorldYLength String
    | UpdateColorSchemeAlive String
    | UpdateColorSchemeDead String
    | UpdateColorSchemeBackground String
    | UpdateRule String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandom ->
            ( model
            , Random.generate
                Reset
                (Random.Array.array model.worldLength.y <|
                    Random.Array.array model.worldLength.x <|
                        Random.map (\n -> n < model.density) <|
                            Random.int 0 99
                )
            )

        Reset newCells ->
            ( { model
                | cells =
                    Array.indexedMap
                        (\yIndex xCells ->
                            Array.indexedMap
                                (\xIndex cell ->
                                    if
                                        xIndex
                                            >= model.margin
                                            && xIndex
                                            < model.worldLength.x
                                            - model.margin
                                            && yIndex
                                            >= model.margin
                                            && yIndex
                                            < model.worldLength.y
                                            - model.margin
                                    then
                                        cell

                                    else
                                        False
                                )
                                xCells
                        )
                        newCells
                , generation = 0
                , isAutomated = False
              }
            , Cmd.none
            )

        Tick ->
            ( { model | cells = Cell.resolve model.worldLength model.cells model.rule, generation = model.generation + 1 }
            , Cmd.none
            )

        Start ->
            ( { model | isAutomated = True }
            , Cmd.none
            )

        Stop ->
            ( { model | isAutomated = False }
            , Cmd.none
            )

        UpdateRefreshInterval interval ->
            ( { model
                | refreshInterval =
                    case String.toFloat interval of
                        Just string ->
                            string

                        Nothing ->
                            model.refreshInterval
              }
            , Cmd.none
            )

        UpdateCellSize size ->
            let
                sizeInt =
                    case String.toInt size of
                        Just int ->
                            int

                        Nothing ->
                            model.cellLook.size
            in
            ( { model | cellLook = { size = sizeInt, shape = model.cellLook.shape } }
            , Cmd.none
            )

        UpdateCellShape shape ->
            ( { model | cellLook = { size = model.cellLook.size, shape = cellShapeFromString shape } }
            , Cmd.none
            )

        UpdateDensity density ->
            let
                densityInt =
                    case String.toInt density of
                        Just int ->
                            int

                        Nothing ->
                            model.density
            in
            ( { model | density = densityInt }
            , Cmd.none
            )

        UpdateMargin margin ->
            let
                marginInt =
                    case String.toInt margin of
                        Just int ->
                            int

                        Nothing ->
                            model.margin
            in
            ( { model | margin = marginInt }
            , Cmd.none
            )

        UpdateWorldXLength length ->
            let
                lengthInt =
                    case String.toInt length of
                        Just int ->
                            int

                        Nothing ->
                            model.worldLength.x
            in
            ( { model | worldLength = { x = lengthInt, y = model.worldLength.y } }
            , Cmd.none
            )

        UpdateWorldYLength length ->
            let
                lengthInt =
                    case String.toInt length of
                        Just int ->
                            int

                        Nothing ->
                            model.worldLength.y
            in
            ( { model | worldLength = { x = model.worldLength.x, y = lengthInt } }
            , Cmd.none
            )

        UpdateColorSchemeAlive color ->
            ( { model
                | colorScheme =
                    { alive = color
                    , dead = model.colorScheme.dead
                    , background = model.colorScheme.background
                    }
              }
            , Cmd.none
            )

        UpdateColorSchemeDead color ->
            ( { model
                | colorScheme =
                    { alive = model.colorScheme.alive
                    , dead = color
                    , background = model.colorScheme.background
                    }
              }
            , Cmd.none
            )

        UpdateColorSchemeBackground color ->
            ( { model
                | colorScheme =
                    { alive = model.colorScheme.alive
                    , dead = model.colorScheme.dead
                    , background = color
                    }
              }
            , Cmd.none
            )

        UpdateRule rule ->
            ( { model | rule = Cell.ruleFromString rule }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isAutomated then
        Time.every model.refreshInterval (\posix -> Tick)

    else
        Sub.none



-- VIEW


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    Html.Events.on "change" (Json.Decode.map handler Html.Events.targetValue)


calcRounding : CellLook -> Int
calcRounding look =
    case look.shape of
        Square ->
            0

        Squircle ->
            look.size // 4

        Circle ->
            look.size // 2


cellColor : Cell.Cell -> ColorScheme -> String
cellColor cell colorScheme =
    if cell then
        colorScheme.alive

    else
        colorScheme.dead


toSvg : Cell.Coordinates -> Cell.Cell -> CellLook -> ColorScheme -> Svg msg
toSvg coordinates cell cellLook colorScheme =
    Svg.rect
        [ Svg.Attributes.x <| String.fromInt <| cellLook.size * coordinates.x
        , Svg.Attributes.y <| String.fromInt <| cellLook.size * coordinates.y
        , Svg.Attributes.rx <| String.fromInt <| calcRounding cellLook
        , Svg.Attributes.ry <| String.fromInt <| calcRounding cellLook
        , Svg.Attributes.width <| String.fromInt cellLook.size
        , Svg.Attributes.height <| String.fromInt cellLook.size
        , Svg.Attributes.fill <| cellColor cell colorScheme
        ]
        []


view : Model -> Html.Html Msg
view model =
    let
        worldSize =
            { x = String.fromInt <| model.cellLook.size * model.worldLength.x
            , y = String.fromInt <| model.cellLook.size * model.worldLength.y
            }

        cellShapeHandler selectedValue =
            UpdateCellShape selectedValue

        ruleHandler selectedValue =
            UpdateRule selectedValue
    in
    Html.div []
        [ Html.ul []
            [ Html.li []
                [ Html.button [ Html.Events.onClick GenerateRandom ] [ Html.text "Reset" ]
                , Html.button [ Html.Events.onClick Tick ] [ Html.text "Tick" ]
                , Html.button [ Html.Events.onClick Start ] [ Html.text "Start" ]
                , Html.button [ Html.Events.onClick Stop ] [ Html.text "Stop" ]
                ]
            , Html.li []
                [ Html.label [ Html.Attributes.for "refreshInterval" ] [ Html.text "Refresh interval (Milliseconds): " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "refreshInterval"
                    , Html.Attributes.value <| String.fromFloat model.refreshInterval
                    , Html.Events.onInput UpdateRefreshInterval
                    ]
                    []
                ]
            , Html.li []
                [ Html.p [] [ Html.text "Cells" ]
                , Html.label [ Html.Attributes.for "cellSize" ] [ Html.text "size: " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "cellSize"
                    , Html.Attributes.value <| String.fromInt model.cellLook.size
                    , Html.Events.onInput UpdateCellSize
                    ]
                    []
                , Html.label [ Html.Attributes.for "cellShape" ] [ Html.text "shape: " ]
                , Html.select [ onChange cellShapeHandler ]
                    [ Html.option
                        [ Html.Attributes.value <| cellShapeToString Square ]
                        [ Html.text <| cellShapeToString Square ]
                    , Html.option
                        [ Html.Attributes.value <| cellShapeToString Squircle ]
                        [ Html.text <| cellShapeToString Squircle ]
                    , Html.option
                        [ Html.Attributes.value <| cellShapeToString Circle ]
                        [ Html.text <| cellShapeToString Circle ]
                    ]
                ]
            , Html.li []
                [ Html.label [ Html.Attributes.for "cellsDensity" ] [ Html.text "Cells density (%): " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "cellsDensity"
                    , Html.Attributes.value <| String.fromInt model.density
                    , Html.Events.onInput UpdateDensity
                    ]
                    []
                ]
            , Html.li []
                [ Html.label [ Html.Attributes.for "cellsMargin" ] [ Html.text "Cells margin (unit): " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "cellsMargin"
                    , Html.Attributes.value <| String.fromInt model.margin
                    , Html.Events.onInput UpdateMargin
                    ]
                    []
                ]
            , Html.li []
                [ Html.p [] [ Html.text "World length" ]
                , Html.label [ Html.Attributes.for "worldXLength" ] [ Html.text "x: " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "worldXLength"
                    , Html.Attributes.value <| String.fromInt model.worldLength.x
                    , Html.Events.onInput UpdateWorldXLength
                    ]
                    []
                , Html.label [ Html.Attributes.for "worldYLength" ] [ Html.text "y: " ]
                , Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "worldYLength"
                    , Html.Attributes.value <| String.fromInt model.worldLength.y
                    , Html.Events.onInput UpdateWorldYLength
                    ]
                    []
                ]
            , Html.li []
                [ Html.p [] [ Html.text "Colorscheme" ]
                , Html.label [ Html.Attributes.for "colorSchemeAlive" ] [ Html.text "Alive cells: " ]
                , Html.input
                    [ Html.Attributes.type_ "color"
                    , Html.Attributes.id "colorSchemeAlive"
                    , Html.Attributes.value model.colorScheme.alive
                    , Html.Events.onInput UpdateColorSchemeAlive
                    ]
                    []
                , Html.label [ Html.Attributes.for "colorSchemeDead" ] [ Html.text "Dead cells: " ]
                , Html.input
                    [ Html.Attributes.type_ "color"
                    , Html.Attributes.id "colorSchemeDead"
                    , Html.Attributes.value model.colorScheme.dead
                    , Html.Events.onInput UpdateColorSchemeDead
                    ]
                    []
                , Html.label [ Html.Attributes.for "colorSchemeBackground" ] [ Html.text "Background: " ]
                , Html.input
                    [ Html.Attributes.type_ "color"
                    , Html.Attributes.id "colorSchemeBackground"
                    , Html.Attributes.value model.colorScheme.background
                    , Html.Events.onInput UpdateColorSchemeBackground
                    ]
                    []
                ]
            , Html.li []
                [ Html.label [ Html.Attributes.for "cellRule" ] [ Html.text "Rule: " ]
                , Html.select [ onChange ruleHandler ]
                    [ Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.Life ]
                        [ Html.text <| Cell.ruleToString Cell.Life ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.DayAndNight ]
                        [ Html.text <| Cell.ruleToString Cell.DayAndNight ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.Flock ]
                        [ Html.text <| Cell.ruleToString Cell.Flock ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.HighLife ]
                        [ Html.text <| Cell.ruleToString Cell.HighLife ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.LongLife ]
                        [ Html.text <| Cell.ruleToString Cell.LongLife ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.Maze ]
                        [ Html.text <| Cell.ruleToString Cell.Maze ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.Move ]
                        [ Html.text <| Cell.ruleToString Cell.Move ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.Seeds ]
                        [ Html.text <| Cell.ruleToString Cell.Seeds ]
                    , Html.option
                        [ Html.Attributes.value <| Cell.ruleToString Cell.TwoXTwo ]
                        [ Html.text <| Cell.ruleToString Cell.TwoXTwo ]
                    ]
                ]
            ]
        , Html.div []
            [ Html.text "Generation: "
            , Html.text <| String.fromInt model.generation
            ]
        , Html.div []
            [ Svg.svg
                [ Svg.Attributes.width worldSize.x
                , Svg.Attributes.height worldSize.y
                , Svg.Attributes.viewBox <| "0 0 " ++ worldSize.x ++ " " ++ worldSize.y
                ]
                ([ Svg.rect
                    [ Svg.Attributes.x "0"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.width worldSize.x
                    , Svg.Attributes.height worldSize.y
                    , Svg.Attributes.fill model.colorScheme.background
                    ]
                    []
                 ]
                    ++ (List.concat <|
                            List.map Array.toList <|
                                Array.toList <|
                                    Array.indexedMap
                                        (\yIndex xCells ->
                                            Array.indexedMap
                                                (\xIndex cell -> toSvg { x = xIndex, y = yIndex } cell model.cellLook model.colorScheme)
                                                xCells
                                        )
                                        model.cells
                       )
                )
            ]
        ]
