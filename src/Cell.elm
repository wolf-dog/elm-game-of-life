module Cell exposing (Cell, Cells, Coordinates, Rule(..), resolve, ruleToString, ruleFromString)

import Array exposing (Array)

type alias Cell = Bool

type alias Cells = Array (Array Cell)

type alias Coordinates =
  { x : Int
  , y : Int
  }

type Rule
  = Life
  | DayAndNight
  | Flock
  | HighLife
  | LongLife
  | Maze
  | Move
  | Seeds
  | TwoXTwo

resolve : Coordinates -> Cells -> Rule -> Cells
resolve length cells rule =
  Array.indexedMap
    (
      \yIndex xCells ->
        Array.indexedMap
          (
            \xIndex cell ->
              resolveCell { x = xIndex, y = yIndex } cell length cells rule
          )
          xCells
    )
    cells

resolveCell : Coordinates -> Cell -> Coordinates -> Cells -> Rule -> Cell
resolveCell  position cell length cells rule =
  let
    neighborsPositions =
      [ { x = position.x - 1, y = position.y - 1 }
      , { x = position.x - 1, y = position.y }
      , { x = position.x - 1, y = position.y + 1 }
      , { x = position.x, y = position.y - 1 }
      , { x = position.x, y = position.y + 1 }
      , { x = position.x + 1, y = position.y - 1 }
      , { x = position.x + 1, y = position.y }
      , { x = position.x + 1, y = position.y + 1 }
      ]
    numOfAliveNeighbors =
      List.sum <|
        List.map (\isAlive -> if isAlive then 1 else 0) <|
        List.map (\neighborsPosition -> isCellAlive neighborsPosition cells) neighborsPositions
  in
    if rule == Life then
      resolveInLife cell numOfAliveNeighbors
    else if rule == DayAndNight then
      resolveInDayAndNight cell numOfAliveNeighbors
    else if rule == Flock then
      resolveInFlock cell numOfAliveNeighbors
    else if rule == HighLife then
      resolveInHighLife cell numOfAliveNeighbors
    else if rule == LongLife then
      resolveInLongLife cell numOfAliveNeighbors
    else if rule == Maze then
      resolveInMaze cell numOfAliveNeighbors
    else if rule == Move then
      resolveInMove cell numOfAliveNeighbors
    else if rule == Seeds then
      resolveInSeeds cell numOfAliveNeighbors
    else
      resolveInLife cell numOfAliveNeighbors

isCellAlive : Coordinates -> Cells -> Bool
isCellAlive position cells =
  case Array.get position.y cells of
    Just neighbors ->
      case Array.get position.x neighbors of
        Just cell ->
          cell
        Nothing ->
          False
    Nothing -> False

resolveInLife : Cell -> Int -> Cell
resolveInLife cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [2, 3] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3] then
      True
    else
      False

resolveInTwoXTwo : Cell -> Int -> Cell
resolveInTwoXTwo cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [1, 2, 5] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3, 6] then
      True
    else
      False

resolveInDayAndNight : Cell -> Int -> Cell
resolveInDayAndNight cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [3, 4, 6, 7, 8] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3, 6, 7, 8] then
      True
    else
      False

resolveInFlock : Cell -> Int -> Cell
resolveInFlock cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [1, 2] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3] then
      True
    else
      False

resolveInHighLife : Cell -> Int -> Cell
resolveInHighLife cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [2, 3] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3, 6] then
      True
    else
      False

resolveInLongLife : Cell -> Int -> Cell
resolveInLongLife cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [5] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3, 4, 5] then
      True
    else
      False

resolveInMaze : Cell -> Int -> Cell
resolveInMaze cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [1, 2, 3, 4, 5] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3] then
      True
    else
      False

resolveInMove : Cell -> Int -> Cell
resolveInMove cell numOfAliveNeighbors =
  if cell then
    if List.member numOfAliveNeighbors [2, 4, 5] then
      True
    else
      False
  else
    if List.member numOfAliveNeighbors [3, 6, 8] then
      True
    else
      False

resolveInSeeds : Cell -> Int -> Cell
resolveInSeeds cell numOfAliveNeighbors =
  if cell then
    False
  else
    if List.member numOfAliveNeighbors [2] then
      True
    else
      False

ruleToString : Rule -> String
ruleToString rule =
  case rule of
    Life ->
      "Life"
    DayAndNight ->
      "Day & Night"
    Flock ->
      "Flock"
    HighLife ->
      "HighLife"
    LongLife ->
      "LongLife"
    Maze ->
      "Maze"
    Move ->
      "Move"
    Seeds ->
      "Seeds"
    TwoXTwo ->
      "2x2"

ruleFromString : String -> Rule
ruleFromString rule =
  if rule == "Life" then
    Life
  else if rule == "Day & Night" then
    DayAndNight
  else if rule == "Flock" then
    Flock
  else if rule == "HighLife" then
    HighLife
  else if rule == "LongLife" then
    LongLife
  else if rule == "Maze" then
    Maze
  else if rule == "Move" then
    Move
  else if rule == "Seeds" then
    Seeds
  else if rule == "2x2" then
    TwoXTwo
  else
    Life
