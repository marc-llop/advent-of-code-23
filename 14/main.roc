app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        matrix: "../packages/matrix/main.roc",
    }
    imports [cli.Stdout, "test.txt" as input : Str, matrix.Matrix.{Matrix}, cli.Task]
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
        |> Matrix.transpose



countWeight = \len -> \(lastStone, weight), stone, index ->
    # dbg (lastStone, weight)
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

main =
    matrix = parseMatrix input
    _ <- Task.await (printMatrix matrix)
    Matrix.turnRight matrix
        |> printMatrix
    # Stdout.line (weightMatrix matrix |> Num.toStr)