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

energizeRegardlessOfHistory : Matrix Str, Matrix TileHistory, Direction, Point, Str -> Matrix TileHistory
energizeRegardlessOfHistory = \matrix, history, direction, point, tile ->
    # dbg (direction, point)
    newHistory = energizeTile history point direction
    continue = \newDirection, nextHistory ->
        # dbg (point, newDirection)
        when nextPoint newDirection point is
            Ok newPoint -> energize matrix nextHistory newDirection newPoint
            Err _ -> nextHistory
    continueStraight = \_ ->
        when nextPoint direction point is
            Ok newPoint -> energize matrix newHistory direction newPoint
            Err _ -> newHistory

    when tile is
        "|" -> when direction is
            Up | Down -> continueStraight {}
            Left | Right -> 
                historyAfterOneDirection = continue Up newHistory
                continue Down historyAfterOneDirection
        "-" -> when direction is
            Up | Down -> 
                historyAfterOneDirection = continue Left newHistory
                continue Right historyAfterOneDirection
            Left | Right -> continueStraight {}
        "." -> continueStraight {}
        mirror -> continue (nextDirection mirror direction) newHistory

reverseDirection : Direction -> Direction
reverseDirection = \direction ->
    when direction is
        Up -> Down
        Down -> Up
        Right -> Left
        Left -> Right

energize : Matrix Str, Matrix TileHistory, Direction, Point -> Matrix TileHistory
energize = \matrix, history, direction, point ->
    result =
        tile <- Matrix.get matrix point |> Result.try
        tileHistory <- Matrix.get history point |> Result.try
        tileContains = \dir -> Set.contains tileHistory dir
        compute = \_ -> energizeRegardlessOfHistory matrix history direction point tile
        when tile is
            "." -> if tileContains direction || tileContains (reverseDirection direction)
                then Err Break
                else Ok (compute {})

            "|" | "-" -> if isEnergized tileHistory
                then Err Break
                else Ok (compute {})

            _ -> if tileContains direction || tileContains (nextDirection tile direction |> reverseDirection)
                then Err Break
                else Ok (compute {})
            
        Result.withDefault result history

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