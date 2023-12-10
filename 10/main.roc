app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "test.txt" as input : Str]
    provides [main] to cli

Pipe : [Vertical, Horizontal, TopLeft, TopRight, BottomLeft, BottomRight, Start, Nothing]

Point : {x: Nat, y: Nat}

Matrix a : List (List a)

graphemeToPipe : Str -> Pipe
graphemeToPipe = \text ->
    when text is
        "-" -> Horizontal
        "|" -> Vertical
        "J" -> TopLeft
        "L" -> TopRight
        "7" -> BottomLeft
        "F" -> BottomRight
        "S" -> Start
        _ -> Nothing

lineToPipeList : Str -> List Pipe
lineToPipeList = \text ->
    Str.graphemes text
        |> List.map graphemeToPipe

matrixFindFirstIndex : Matrix elem, (elem -> Bool) -> Result Point [NotFound]
matrixFindFirstIndex = \matrix, predicate ->
    List.walkWithIndex matrix (Err NotFound) \found, line, y ->
        if found != Err NotFound
            then found
            else List.findFirstIndex line predicate
                |> Result.map \x -> {x, y}


main =
    matrix = Str.split input "\n"
        |> List.map lineToPipeList
    startPosition = matrixFindFirstIndex matrix \elem -> elem == Start
    dbg startPosition
    Stdout.line "hi"