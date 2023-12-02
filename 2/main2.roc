app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, ResultExtra.{ map2, sequence }]
    provides [main] to cli

Color : [Red, Green, Blue]

ColorSet : (Color, Nat)

CubeSet : Dict Color Nat

Game : { id: Nat, results: List CubeSet }

parseColor : Str -> Result Color [InvalidColor]
parseColor = \text ->
    when text is
        "red" -> Ok Red
        "green" -> Ok Green
        "blue" -> Ok Blue
        _ -> Err InvalidColor

parseColorSet : Str -> Result ColorSet [InvalidColor, InvalidNumStr, InvalidText]
parseColorSet = \text ->
    when Str.split text " " is
        [a, b] -> 
            color <- parseColor b |> Result.try
            amount <- Str.toNat a |> Result.try
            Ok (color, amount)
        _ -> Err InvalidText

colorSetListToCubeSet : List ColorSet -> CubeSet
colorSetListToCubeSet = \colorSets ->
    Dict.fromList colorSets

parseCubeSet : Str -> Result CubeSet [InvalidColor, InvalidNumStr, InvalidText]
parseCubeSet = \text ->
    Str.split text ","
        |> List.map Str.trim
        |> List.map parseColorSet
        |> sequence
        |> Result.map colorSetListToCubeSet

parseCubeSets : Str -> Result (List CubeSet) [InvalidColor, InvalidNumStr, InvalidText]
parseCubeSets = \text ->
    Str.split text ";"
        |> List.map Str.trim
        |> List.map parseCubeSet
        |> sequence

parseGameId : Str -> Result Nat [InvalidNumStr]
parseGameId = \text ->
    Str.replaceFirst text "Game " ""
        |> Str.toNat

parseGame : Str -> Result Game [InvalidColor, InvalidNumStr, InvalidText]
parseGame = \text ->
    newGame : Nat, List CubeSet -> Game
    newGame = \id, cubeSets -> {id: id, results: cubeSets}
    when Str.split text ":" is
        [gameIdTag, results] -> map2 newGame (parseGameId gameIdTag) (parseCubeSets results)
        _ -> Err InvalidText

power : CubeSet -> Nat
power = \cubeSet ->
    Dict.walk cubeSet 1 (\pow, _, cubes -> pow * cubes)

minimumCubesOfColor : Color, List CubeSet -> Nat
minimumCubesOfColor = \color, cubeSets ->
    List.keepOks cubeSets (\cubeSet -> Dict.get cubeSet color)
        |> List.max
        |> Result.withDefault 0

minimumCubesInGame : Game -> CubeSet
minimumCubesInGame = \{results} ->
    Dict.fromList [
        (Red, minimumCubesOfColor Red results),
        (Green, minimumCubesOfColor Green results),
        (Blue, minimumCubesOfColor Blue results)
    ]

main =
    sum = Str.split input "\n"
        |> List.keepOks parseGame
        |> List.map (\game -> minimumCubesInGame game |> power)
        |> List.sum
    Stdout.line (Num.toStr sum)
