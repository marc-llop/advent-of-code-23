app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        matrix: "../packages/matrix/main.roc",
        resultExtra: "../packages/result-extra/main.roc",
    }
    imports [cli.Stdout, "input.txt" as input : Str, matrix.Matrix.{Matrix}, cli.Task]
    provides [main] to cli

#   X987654321
#   OO.O.O..##
#   ...OO....O
#   .O...#O..O
#   .O.#......
#   .#.O......
#   #.#..O#.##
#   ..#...O.#.
#   ....O#.O#.
#   ....#.....
#   .#.O.#O...

parseMatrix = \text -> 
    Str.trim text
        |> Str.split "\n" 
        |> List.map Str.graphemes
        # |> Matrix.transpose

countWeight = \len -> \(lastStone, weight), stone, index ->
    when stone is
        "O" -> (lastStone + 1, weight + (len - lastStone))
        "#" -> (index + 1, weight)
        _ -> (lastStone, weight)

weightRow = \row ->
    (_, weight) = List.walkWithIndex row (0, 0) (countWeight (List.len row))
    weight

weightMatrix = \matrix ->
    List.map matrix weightRow
        |> List.sum

printMatrix = \matrix ->
    _ <- Matrix.toStr matrix
        |> Stdout.line
        |> Task.await
    Stdout.line ""

gravitize : (List Str, Nat), Str, Nat -> (List Str, Nat)
gravitize = \(fallenStones, lastHole), stone, index ->
    rolledStone = List.set fallenStones index "."
        |> List.set lastHole "O"
    when stone is
        "O" -> (rolledStone, lastHole + 1)
        "#" -> (fallenStones, index + 1)
        _ -> (fallenStones, lastHole)

rollWest = \matrix ->
    List.map matrix \row -> 
        (newRow, _) = List.walkWithIndex row (row, 0) gravitize
        newRow

rollNorth = \matrix ->
    Matrix.mapColumnsWithIndex matrix \column, _ ->
        (newColumn, _) = List.walkWithIndex column (column, 0) gravitize
        newColumn

rollEast = \matrix ->
    List.map matrix \row ->
        reversedRow = List.reverse row
        (newRow, _) = List.walkWithIndex reversedRow (reversedRow, 0) gravitize
        List.reverse newRow

rollSouth = \matrix ->
    Matrix.mapColumnsWithIndex matrix \column, _ ->
        reversedColumn = List.reverse column
        (newColumn, _) = List.walkWithIndex reversedColumn (reversedColumn, 0) gravitize
        List.reverse newColumn

cycle = \matrix ->
    matrix
        |> rollNorth
        |> rollWest
        |> rollSouth
        |> rollEast



repeatCycle = \matrix, times, cache ->
    when Dict.get cache matrix is
        Ok computedMatrix ->
            dbg times
            computedMatrix
        Err _ ->
            nextMatrix = cycle matrix
            if times == 0
                then
                    dbg times 
                    matrix
                else repeatCycle nextMatrix (times - 1) (Dict.insert cache matrix nextMatrix)

main =
    matrix = parseMatrix input
    # _ <- Task.await (printMatrix matrix)
    repeatCycle matrix 1000000000 (Dict.empty {})
        |> printMatrix
    # Stdout.line "hi"
    # Stdout.line (weightMatrix matrix |> Num.toStr)