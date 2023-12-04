app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

parseNumberList : Str -> List Nat
parseNumberList = \text ->
    Str.trim text
        |> Str.split " "
        |> List.keepOks Str.toNat

parseCard : Str -> {winners: List Nat, owned: List Nat}
parseCard = \text ->
    parts = Str.split text ":"
        |> List.get 1
        |> Result.withDefault ""
        |> Str.split "|"
    when parts is
        [winnerPart, ownedPart] -> {winners: parseNumberList winnerPart, owned: parseNumberList ownedPart}
        _ -> {winners: [], owned: []}

countPoints : (List Nat, Nat), Nat -> (List Nat, Nat)
countPoints = \(ownedNumbers, points), winner ->
    newPoints = if List.contains ownedNumbers winner
        then when points is
            0 -> 1
            _ -> points * 2
        else points
    (ownedNumbers, newPoints)

cardWorth : {winners: List Nat, owned: List Nat} -> Nat
cardWorth = \{winners, owned} ->
    (_, points) = List.walk winners (owned, 0) countPoints
    points

main =
    lines = Str.split input "\n"
    sum = List.map lines parseCard
        |> List.map cardWorth
        |> List.sum
        |> Num.toStr
    Stdout.line sum