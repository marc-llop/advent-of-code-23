app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "../packages/roc-parser/package/main.roc",
    }
    imports [cli.Stdout, "test.txt" as input : Str, parser.Core.{const, keep, skip, chompUntil, oneOf, map, flatten}, parser.String.{parseStr, digit, string}]
    provides [main] to cli

Label : Str

Lens : {label: Label, focalLength: Nat}

Step : [Insert Lens, Remove Label]

stepToStr = \step ->
    when step is
        Insert {label, focalLength} -> "Insert \(label) \(Num.toStr focalLength)"
        Remove label -> "Remove \(label)"

parseStep : Str -> Result Step [ParsingError]
parseStep = \text ->
    toStringParser = \u8s ->
        when Str.fromUtf8 u8s is
            Ok str -> Ok str
            Err _ -> Err ""

    parser = oneOf [
        const (\label -> \focalLength -> Insert {label, focalLength})
            |> keep (chompUntil '=' |> map toStringParser |> flatten)
            |> skip (string "=")
            |> keep digit,
        const (\label -> Remove label)
            |> keep (chompUntil '-' |> map toStringParser |> flatten)
            |> skip (string "-")
    ]

    parseStr parser text
        |> Result.mapErr \_ -> ParsingError

parseInput : Str -> List Step
parseInput = \text ->
    Str.replaceEach text "\n" ""
        |> Str.split ","
        |> List.keepOks parseStep

hash : Step -> Nat
hash = \step ->
    stepLabel = when step is
        Insert {label} -> label
        Remove label -> label
    labelScalars = Str.toScalars stepLabel
        |> List.map Num.toNat
    List.walk labelScalars 0 \currentValue, character ->
        currentValue
            |> \v -> v + character
            |> \v -> v * 17
            |> \v -> v % 256


LensBox : (Dict Str Nat, List Lens)

insertLens : LensBox, Lens -> LensBox
insertLens = \(positionDict, lenses), lens ->
    when Dict.get positionDict lens.label is
        Ok index -> (positionDict, List.set lenses index lens)
        Err _ -> (
            Dict.insert positionDict lens.label (List.len lenses),
            List.append lenses lens,
        )

removeLens : LensBox, Lens -> LensBox
removeLens = \(positionDict, lenses), lens ->
    when Dict.get positionDict lens.label is
        Ok index -> (
            Dict.remove positionDict lens.label,
            List.dropAt lenses index,
        )
        Err _ -> (positionDict, lenses)

main =
    hashes = parseInput input
    List.map hashes stepToStr
        |> Str.joinWith "\n"
        |> Stdout.line
        # |> List.map hash
    # hashSum = List.sum hashes
    # Stdout.line (Num.toStr hashSum) 