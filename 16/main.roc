app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        matrix: "../packages/matrix/main.roc",
    }
    imports [cli.Stdout, "input.txt" as input : Str, matrix.Matrix.{Matrix, Point}]
    provides [main] to cli

parseMatrix = \text ->
    Str.trim text
        |> Str.split "\n"
        |> List.map Str.graphemes

Direction : [Up, Down, Right, Left]

nextPoint : Direction, Point -> Result Point [OutOfBounds]
nextPoint = \direction, {x, y} ->
    when direction is
        Up -> if y > 0 
            then Ok {x, y: y - 1}
            else Err OutOfBounds
        Down -> Ok {x, y: y + 1}
        Left -> if x > 0
            then Ok {x: x - 1, y}
            else Err OutOfBounds
        Right -> Ok {x: x + 1, y}

TileHistory : Set Direction

energizeTile : Matrix TileHistory, Point, Direction -> Matrix TileHistory
energizeTile = \matrix, point, direction ->
    Matrix.update matrix point \directions ->
        Set.insert directions direction


joinHistories : TileHistory, TileHistory -> TileHistory
joinHistories = \a, b ->
    Set.union a b

isEnergized : TileHistory -> Bool
isEnergized = \directions -> !(Set.isEmpty directions)

# Does not handle splitters
nextDirection : Str, Direction -> Direction
nextDirection = \tile, direction ->
    when tile is
        "/" -> when direction is
            Up -> Right
            Right -> Up
            Down -> Left
            Left -> Down
        "\\" -> when direction is
            Up -> Left
            Left -> Up
            Down -> Right
            Right -> Down
        "." -> direction
        _ -> direction

energizeRegardlessOfHistory : Matrix Str, Matrix TileHistory, Direction, Point -> Matrix TileHistory
energizeRegardlessOfHistory = \matrix, history, direction, point ->
    # dbg (direction, point)
    newHistory = energizeTile history point direction
    continue = \newDirection ->
        # dbg (point, newDirection)
        when nextPoint newDirection point is
            Ok newPoint -> energize matrix newHistory newDirection newPoint
            Err _ -> newHistory
    continueStraight = \_ ->
        when nextPoint direction point is
            Ok newPoint -> energize matrix newHistory direction newPoint
            Err _ -> newHistory

    maybeTile = Matrix.get matrix point
    when maybeTile is
        Err _ -> history
        Ok "|" -> when direction is
            Up | Down -> continueStraight {}
            Left | Right -> Matrix.map2 (continue Up) (continue Down) joinHistories
        Ok "-" -> when direction is
            Up | Down -> Matrix.map2 (continue Left) (continue Right) joinHistories
            Left | Right -> continueStraight {}
        Ok "." -> continueStraight {}
        Ok mirror -> continue (nextDirection mirror direction)

reverseDirection : Direction -> Direction
reverseDirection = \direction ->
    when direction is
        Up -> Down
        Down -> Up
        Right -> Left
        Left -> Right

energize : Matrix Str, Matrix TileHistory, Direction, Point -> Matrix TileHistory
energize = \matrix, history, direction, point ->
    maybeTile = Matrix.get matrix point
    maybeTileHistory = Matrix.get history point
    when (maybeTile, maybeTileHistory) is
        (Ok tile, Ok tileHistory) ->
            splittedBefore = when tile is
                "|" | "-" -> isEnergized tileHistory
                _ -> Bool.false

            cameFromThere = when tile is
                "/" | "\\" | "." -> Set.contains tileHistory (nextDirection tile direction |> reverseDirection)
                _ -> Bool.false

            passedHereInTheSameDirection = Set.contains tileHistory direction

            beenThereDoneThat = splittedBefore || cameFromThere || passedHereInTheSameDirection

            if beenThereDoneThat
                then history
                else energizeRegardlessOfHistory matrix history direction point
            
        _ -> history

initialHistory = \matrix ->
    length = Matrix.len matrix
    List.repeat (Set.empty {}) length.x
        |> List.repeat length.y

main =
    matrix = parseMatrix input
    energized = energize matrix (initialHistory matrix) Right {x: 0, y: 0}
    # energizedStrs = Matrix.map energized \a -> if isEnergized a then "#" else "."
    # Stdout.line (Matrix.toStr energizedStrs)
    energizedTiles = Matrix.countIf energized isEnergized
    Stdout.line (Num.toStr energizedTiles)