app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, ResultExtra.{sequence}]
    provides [main] to cli

Card : [N Nat, T, J, Q, K, A]

compareCards : Card, Card -> [LT, EQ, GT]
compareCards = \a, b ->
    if a == b
        then EQ
        else when (a, b) is
            (A, _) -> GT
            (K, A) -> LT
            (K, _) -> GT
            (Q, A) -> LT
            (Q, K) -> LT
            (Q, _) -> GT
            (J, N _) -> GT
            (J, T) -> GT
            (J, _) -> LT
            (T, N _) -> GT
            (T, _) -> LT
            (N x, N y) -> Num.compare x y
            (N _, _) -> LT

parseCard : Str -> Result Card [InvalidCard]
parseCard = \text ->
    when text is
        "2" -> Ok (N 2)
        "3" -> Ok (N 3)
        "4" -> Ok (N 4)
        "5" -> Ok (N 5)
        "6" -> Ok (N 6)
        "7" -> Ok (N 7)
        "8" -> Ok (N 8)
        "9" -> Ok (N 9)
        "T" -> Ok T
        "J" -> Ok J
        "Q" -> Ok Q
        "K" -> Ok K
        "A" -> Ok A
        _ -> Err InvalidCard

parseHand : Str -> Result (List Card) [InvalidCard]
parseHand = \text ->
    Str.graphemes text
        |> List.map parseCard
        |> sequence

parseBid : Str -> Result (List Card, Nat) [InvalidBid]
parseBid = \text ->
    when Str.split text " " is
        [a, b] -> 
            parseHand a |> Result.try \hand ->
                Str.toNat b |> Result.map \bid -> (hand, bid)
            |> Result.mapErr \_ -> InvalidBid
        _ -> Err InvalidBid

HandType : [Recamel, Camel, Tabernacle, Triplet, DoublePair, Pair, HighCard]

compareHandTypes : HandType, HandType -> [LT, EQ, GT]
compareHandTypes = \a, b ->
    if a == b
        then EQ
        else when (a, b) is
            (Recamel, _) -> GT
            (Camel, Recamel) -> LT
            (Camel, _) -> GT
            (Tabernacle, Recamel) -> LT
            (Tabernacle, Camel) -> LT
            (Tabernacle, _) -> GT
            (Triplet, Recamel) -> LT
            (Triplet, Camel) -> LT
            (Triplet, Tabernacle) -> LT
            (Triplet, _) -> GT
            (DoublePair, Pair) -> GT
            (DoublePair, HighCard) -> GT
            (DoublePair, _) -> LT
            (Pair, HighCard) -> GT
            (Pair, _) -> LT
            (HighCard, _) -> LT

count : List a -> Dict a Nat
count = \list ->
    dict : Dict a Nat
    dict = Dict.empty {}
    List.walk list dict \groups, currentElem ->
        Dict.update groups currentElem \maybeAmount ->
            when maybeAmount is
                Present amount -> Present (amount + 1)
                Missing -> Present 1

handType : List Card -> HandType
handType = \cards ->
    cardGroups = count cards
    differentCards = Dict.len cardGroups
    groupSizes = Dict.values cardGroups |> List.sortDesc
    when (differentCards, groupSizes) is
        (1, _) -> Recamel
        (2, [4, 1]) -> Camel
        (2, _) -> Tabernacle
        (3, [3, 1, 1]) -> Triplet
        (3, _) -> DoublePair
        (4, _) -> Pair
        _ -> HighCard

expect handType [N 3, N 2, T, N 3, K] == Pair
expect handType [T, N 5, N 5, J, N 5] == Triplet
expect handType [K, K, N 6, N 7, N 7] == DoublePair
expect handType [K, T, J, J, T] == DoublePair
expect handType [Q, Q, Q, J, A] == Triplet
expect handType [A, A, A, A, A] == Recamel
expect handType [J, J, J, J, N 4] == Camel
expect handType [N 2, N 3, N 5, T, J] == HighCard

compareHighCard : List Card, List Card -> [LT, EQ, GT]
compareHighCard = \a, b ->
    List.map2 a b compareCards
        |> List.findFirst \res -> res != EQ
        |> Result.withDefault EQ

compareHands : List Card, List Card -> [LT, EQ, GT]
compareHands = \a, b ->
    typeA = handType a
    typeB = handType b
    when compareHandTypes typeA typeB is
        EQ -> compareHighCard a b
        res -> res

main =
    lines = Str.split input "\n"
    bids = List.keepOks lines parseBid
    sortedBids = List.sortWith bids \(handA, _), (handB, _) -> compareHands handA handB
    totalWinnings = List.walkWithIndex sortedBids 0 \winnings, (_, bid), index ->
        rank = index + 1
        winnings + (bid * rank)
    Stdout.line (Num.toStr totalWinnings)