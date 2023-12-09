app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

equalsZero = \x -> x == 0

# scanl : List a, b, (b, a -> b) -> List b
# scanl = \list, initialState, fn ->
#     scan1 : List b, a -> List b
#     scan1 = \accList, x ->
#         when accList is
#             [] -> []
#             [acc, ..] -> List.append accList (fn acc x)
#     List.reverse (List.walk list [initialState] scan1)

# scanl1 : List a, (a, a -> a) -> List a
# scanl1 = \list, fn ->
#     when list is
#         [] -> []
#         [first, ..] -> scanl (List.dropFirst list 1) first fn

pairs : List a -> List (a, a)
pairs = \list ->
    when list is
        [a, b, ..] -> List.concat [(a, b)] (pairs (List.dropFirst list 1))
        _ -> []

computeIncrements : List I64 -> List I64
computeIncrements = \list ->
    pairs list
        |> List.map \(a, b) -> b - a

expect computeIncrements [0, 3, 6, 9, 12, 15] == [3, 3, 3, 3, 3]
expect computeIncrements [3, 3, 3, 3, 3] == [0, 0, 0, 0]
expect computeIncrements [1, 3, 6, 10, 15, 21] == [2, 3, 4, 5, 6]
expect computeIncrements [2, 3, 4, 5, 6] == [1, 1, 1, 1]
expect computeIncrements [10, 13, 16, 21, 30, 45, 68] == [3, 3, 5, 9, 15, 23]
expect computeIncrements [3, 3, 5, 9, 15, 23] == [0, 2, 4, 6, 8]
expect computeIncrements [0, 2, 4, 6, 8] == [2, 2, 2, 2]

guessNextValue : List I64 -> Result I64 [ListWasEmpty]
guessNextValue = \list ->
    increments = computeIncrements list
    if List.all increments equalsZero
        then List.last list
        else 
            lastIncrement <- guessNextValue increments |> Result.try
            lastValue <- List.last list |> Result.map
            (lastValue + lastIncrement)

main =
    lines = Str.split input "\n"
    sequences = List.map lines
        \line -> Str.split line " " 
            |> List.keepOks Str.toI64
    # dbg sequences
    nextValues = List.keepOks sequences guessNextValue
    # dbg nextValues
    solution = List.sum nextValues
    Stdout.line (Num.toStr solution)