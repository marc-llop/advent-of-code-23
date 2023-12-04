app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

parseNumberList : Str -> List Nat
parseNumberList = \text ->
    Str.trim text
        |> Str.split " "
        |> List.keepOks Str.toNat

parseCardContent : Str -> {winners: List Nat, owned: List Nat}
parseCardContent = \text ->
    parts = Str.split text "|"
    when parts is
        [winnerPart, ownedPart] -> {
            winners: parseNumberList winnerPart,
            owned: parseNumberList ownedPart
        }
        _ -> {winners: [], owned: []}

cardWorth : {winners: List Nat, owned: List Nat} -> Nat
cardWorth = \{winners, owned} ->
    winSet = Set.fromList winners
    ownSet = Set.fromList owned
    Set.len (Set.intersection winSet ownSet)

parseCard : Str -> Result Nat [InvalidCard]
parseCard = \text ->
    parts = Str.split text ":"
    when parts is
        [_, cardContent] -> 
            content = parseCardContent cardContent
            Ok (cardWorth content)
        _ -> Err InvalidCard

# [] [1, 1, 1, 1, 1, 1]
# 4 [1] [2, 2, 2, 2, 1]
# 2 [1, 2] [4, 4, 2, 1]
# 2 [1, 2, 4] [8, 6, 1]
# 1 [1, 2, 4, 8] [14, 1]
# 0 [1, 2, 4, 8, 14] [1]
# 0 [1, 2, 4, 8, 14, 1]

countCards : List Nat, Nat, Nat -> List Nat
countCards = \copiesPerCard, winners, cardIndex ->
    start = cardIndex + 1
    end = start + winners
    copiesOfThisCard = List.get copiesPerCard cardIndex
        |> Result.withDefault 0
    List.mapWithIndex copiesPerCard \copies, index ->
        if index >= start && index < end
            then copies + copiesOfThisCard
            else copies

main =
    cards = Str.split (Str.trim input) "\n"
        |> List.keepOks parseCard
    copiesPerCard = List.repeat 1 (List.len cards)
    finalCopiesPerCard = List.walkWithIndex cards copiesPerCard countCards
    dbg finalCopiesPerCard
    sum = List.sum finalCopiesPerCard
        |> Num.toStr
    Stdout.line sum