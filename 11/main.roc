app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, Matrix.{Point}]
    provides [main] to cli

parseRightAscension : List Point, Str, Nat -> List Point
parseRightAscension = \previousGalaxies, text, y ->
    Str.graphemes text
        |> List.walkWithIndex previousGalaxies \galaxies, grapheme, x ->
            if grapheme == "#"
                then List.append galaxies {x, y}
                else galaxies

parseFieldOfView : Str -> List Point
parseFieldOfView = \text ->
    lines = Str.split text "\n"
    List.walkWithIndex lines [] parseRightAscension

correctExpansion : List Point -> List Point
correctExpansion = \galaxies ->
    xIndexes = List.map galaxies .x
    yIndexes = List.map galaxies .y

    columns = List.range {start: At 0, end: Before (List.max xIndexes |> Result.withDefault 0)} 
        |> Set.fromList
    rows = List.range {start: At 0, end: Before (List.max yIndexes |> Result.withDefault 0)}
        |> Set.fromList

    emptyColumns = Set.difference columns (Set.fromList xIndexes)
        |> Set.toList
    emptyRows = Set.difference rows (Set.fromList yIndexes)
        |> Set.toList

    emptyOnesBefore : Nat, List Nat -> Nat
    emptyOnesBefore = \index, empties ->
        List.countIf empties \e -> e < index

    List.map galaxies \{x, y} ->
        {
            x: x + (emptyOnesBefore x emptyColumns),
            y: y + (emptyOnesBefore y emptyRows),
        }

uniquePairs : List a -> List (a, a)
uniquePairs = \list ->
    when list is
        [] -> []
        [x, ..] ->
            rest = List.dropFirst list 1
            elemPairs = List.map rest (\y -> (x, y))
            List.concat elemPairs (uniquePairs rest)

main =
    galaxies = parseFieldOfView input
    correctedGalaxies = correctExpansion galaxies
    sumOfDistances = uniquePairs correctedGalaxies
        |> List.map \(a, b) -> Matrix.manhattanDistance a b
        |> List.sum

    Stdout.line (Num.toStr sumOfDistances)