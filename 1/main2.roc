app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

main =
    # lines = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]
    lines = Str.split input "\n"
    List.keepOks lines lineToCalibration
        |> List.sum
        |> Num.toStr
        |> Stdout.line
        
PartialMatch : {
    text: List Str,
    matchedChars: Nat
}

initialPartialMatches : List PartialMatch
initialPartialMatches =
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        |> List.map (\text -> {text: Str.graphemes text, matchedChars: 0})

matchToNat : Str -> Result Nat [InvalidDigit]
matchToNat = \text ->
    when text is
        "zero" -> Ok 0
        "orez" -> Ok 0
        "one" -> Ok 1
        "eno" -> Ok 1
        "two" -> Ok 2
        "owt" -> Ok 2
        "three" -> Ok 3
        "eerht" -> Ok 3
        "four" -> Ok 4
        "ruof" -> Ok 4
        "five" -> Ok 5
        "evif" -> Ok 5
        "six" -> Ok 6
        "xis" -> Ok 6
        "seven" -> Ok 7
        "neves" -> Ok 7
        "eight" -> Ok 8
        "thgie" -> Ok 8
        "nine" -> Ok 9
        "enin" -> Ok 9
        _ -> Err InvalidDigit
    

Find a b : [Found a, NotFound b]

concatFinds : Find a b, Find a (List b) -> Find a (List b)
concatFinds = \a, b ->
    when (a, b) is
        (Found x, _) -> Found x
        (NotFound _, Found y) -> Found y
        (NotFound x, NotFound y) -> NotFound (List.prepend y x)
        (NotFound x, _) -> NotFound [x]

findFirst : List (Find a b) -> Find a (List b)
findFirst = \list ->
    when list is
        [] -> NotFound []
        [Found a, ..] -> Found a
        [NotFound a, ..] -> concatFinds (NotFound a) (List.dropFirst list 1 |> findFirst)

updateMatch : PartialMatch, Str -> [Found Str, NotFound PartialMatch]
updateMatch = \{text, matchedChars}, char ->
    isNextChar = List.sublist text {start: matchedChars, len: 1} == [char]
    isFirstChar = List.sublist text {start: 0, len: 1} == [char]
    isLastChar = List.len text == matchedChars + 1
    if isNextChar
        then
            if isLastChar then Found (Str.joinWith text "")
            else NotFound {text, matchedChars: matchedChars + 1}
        else if isFirstChar
        then NotFound {text, matchedChars: 1}
        else NotFound {text, matchedChars: 0}

updateMatches : Result Nat (List PartialMatch), Str -> [Break (Result Nat (List PartialMatch)), Continue (Result Nat (List PartialMatch))]
updateMatches = \maybePartialMatches, char ->
    when Str.toNat char is
        Ok nat -> Break (Ok nat)
        Err _ ->
            when maybePartialMatches is
                Ok nat -> Break (Ok nat)

                Err partialMatches ->
                    updatedMatches = List.map partialMatches (\pm -> updateMatch pm char)

                    when findFirst updatedMatches is
                        Found nat -> Break (matchToNat nat |> Result.mapErr \_ -> partialMatches)
                        NotFound matches -> Continue (Err matches)

findFirstDigit : List Str -> Result Nat [NotFound]
findFirstDigit = \chars ->
    List.walkUntil chars (Err initialPartialMatches) updateMatches
        |> Result.mapErr (\_ -> NotFound)

findLastDigit : List Str -> Result Nat [NotFound]
findLastDigit = \chars ->
    invertedPartialMatches = List.map initialPartialMatches (\{text, matchedChars} -> {matchedChars, text: List.reverse text})
    List.walkBackwardsUntil chars (Err invertedPartialMatches) updateMatches
        |> Result.mapErr (\_ -> NotFound)

lineToCalibration : Str -> Result Nat [NotFound]
lineToCalibration = \line ->
    graphemes = Str.graphemes line
    firstNum <- findFirstDigit graphemes |> Result.try
    lastNum <- findLastDigit graphemes |> Result.try
    Ok (firstNum * 10 + lastNum)
